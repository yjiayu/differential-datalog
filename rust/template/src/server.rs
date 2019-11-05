use std::collections::{HashMap, HashSet};
use std::fmt::{Debug, Formatter};
use std::sync::{Arc, Mutex};

use differential_datalog::program::RelId;
use differential_datalog::program::Update;
use differential_datalog::program::Val;
use differential_datalog::DDlog;

use distributed_datalog::Observer;
use distributed_datalog::ObserverBox;
use distributed_datalog::OptionalObserver;
use distributed_datalog::SharedObserver;
use distributed_datalog::UpdatesObservable;

use crate::api::HDDlog;

/// An outlet streams a subset of DDlog tables to an observer.
#[derive(Debug)]
pub struct Outlet<V>
where
    V: Val,
{
    tables: HashSet<RelId>,
    observer: SharedObserver<OptionalObserver<ObserverBox<Update<V>, String>>>,
}

/// A DDlog server wraps a DDlog program, and writes deltas to the
/// outlets. The redirect map redirects input deltas to local tables.
#[derive(Debug)]
pub struct DDlogServer<P>
where
    P: DDlog,
{
    prog: Option<P>,
    outlets: Vec<Outlet<P::Value>>,
    redirect: HashMap<RelId, RelId>,
}

impl<P> DDlogServer<P>
where
    P: DDlog,
{
    /// Create a new server with no outlets.
    pub fn new(prog: P, redirect: HashMap<RelId, RelId>) -> Self {
        Self {
            prog: Some(prog),
            outlets: Vec::new(),
            redirect,
        }
    }

    /// Add a new outlet that streams a subset of the tables.
    pub fn add_stream(
        &mut self,
        tables: HashSet<RelId>,
    ) -> UpdatesObservable<Update<P::Value>, String> {
        let observer = SharedObserver::default();
        let outlet = Outlet {
            tables,
            observer: observer.clone(),
        };
        self.outlets.push(outlet);
        UpdatesObservable { observer }
    }

    /// Remove an outlet.
    pub fn remove_stream(&mut self, stream: UpdatesObservable<Update<P::Value>, String>) {
        self.outlets
            .retain(|o| !Arc::ptr_eq(&o.observer, &stream.observer));
    }

    /// Shutdown the DDlog program and notify listeners of completion.
    pub fn shutdown(&mut self) -> Result<(), String> {
        // TODO: Right now we may error out early if an observer's
        //       `on_completed` fails. In the future we probably want to push
        //       those errors somewhere and then continue.
        if let Some(mut prog) = self.prog.take() {
            for outlet in &mut self.outlets {
                outlet.observer.on_completed()?
            }
            prog.stop()?;
        }
        Ok(())
    }
}

impl<P> Observer<Update<P::Value>, String> for DDlogServer<P>
where
    P: Debug + Send + DDlog,
{
    /// Start a transaction when deltas start coming in.
    fn on_start(&mut self) -> Result<(), String> {
        if let Some(ref mut prog) = self.prog {
            prog.transaction_start()?;

            for outlet in &mut self.outlets {
                outlet.observer.on_start()?
            }
            Ok(())
        } else {
            Ok(())
        }
    }

    /// Commit input deltas to local tables and pass on output deltas to
    /// the listeners on the outlets.
    fn on_commit(&mut self) -> Result<(), String> {
        if let Some(ref mut prog) = self.prog {
            let changes = prog.transaction_commit_dump_changes()?;
            for outlet in &self.outlets {
                let mut observer = outlet.observer.lock().unwrap();
                if observer.is_some() {
                    let mut upds = outlet
                        .tables
                        .iter()
                        .flat_map(|table| {
                            let table = *table as usize;
                            changes.as_ref().get(&table).map(|t| {
                                t.iter().map(move |(val, weight)| {
                                    debug_assert!(*weight == 1 || *weight == -1);
                                    if *weight == 1 {
                                        Update::Insert {
                                            relid: table,
                                            v: val.clone(),
                                        }
                                    } else {
                                        Update::DeleteValue {
                                            relid: table,
                                            v: val.clone(),
                                        }
                                    }
                                })
                            })
                        })
                        .flatten()
                        .peekable();

                    if upds.peek().is_some() {
                        observer.on_updates(Box::new(upds))?;
                    }
                    observer.on_commit()?;
                }
            }
            Ok(())
        } else {
            Ok(())
        }
    }

    /// Apply a series of updates.
    fn on_updates<'a>(
        &mut self,
        updates: Box<dyn Iterator<Item = Update<P::Value>> + 'a>,
    ) -> Result<(), String> {
        if let Some(ref prog) = self.prog {
            prog.apply_valupdates(updates.map(|upd| match upd {
                Update::Insert { relid: relid, v: v } => Update::Insert {
                    relid: *self.redirect.get(&relid).unwrap_or(&relid),
                    v,
                },
                Update::DeleteValue { relid: relid, v: v } => Update::DeleteValue {
                    relid: *self.redirect.get(&relid).unwrap_or(&relid),
                    v,
                },
                update => panic!("Operation {:?} not allowed", update),
            }))
        } else {
            Ok(())
        }
    }

    fn on_completed(&mut self) -> Result<(), String> {
        Ok(())
    }
}

impl<P> Drop for DDlogServer<P>
where
    P: DDlog,
{
    /// Shutdown the DDlog program and notify listeners of completion.
    fn drop(&mut self) {
        let _ = self.shutdown();
    }
}
