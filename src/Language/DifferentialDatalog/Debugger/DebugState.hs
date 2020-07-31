{-
Copyright (c) 2018-2020 VMware, Inc.
SPDX-License-Identifier: MIT

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
-}

{-# LANGUAGE TupleSections, LambdaCase, RecordWildCards#-}

module Language.DifferentialDatalog.Debugger.DebugState (
    queryDerivations,
    constructRecordMap,
    handleDebugEvents,
    emptyDebuggerMaps,
    getPredecessorOpId,
    DebuggerMaps(..),
    DebuggerRecord(..),
    OperatorInput(..),
    DebuggerRecordMap,
    )where

import qualified Data.Map as M

import Language.DifferentialDatalog.Syntax
import Language.DifferentialDatalog.Debugger.DebugTypes

-- (1) Derivation corresponds to the input records for a specifc output record.
-- (2) One derivation could have at most two elements since Join has at most two input
--     records and other operators have only one input record.
-- (3) One output record may have multiple derivations.
type Derivation = [DebuggerRecord]

-- Output Record -> all possible derivations
type DebuggerRecordMap = M.Map DebuggerRecord [Derivation]
type DebuggerRecordWeightMap = M.Map DebuggerRecord Int

data OperatorInput = InputOp OperatorId
                   | InputRel String
                   deriving (Show, Eq, Ord)

data DebuggerRecord = DebuggerRecord { dbgRecord :: Record
                                     , dbgOperatorId :: OperatorInput
                                     } deriving (Show, Eq, Ord)

-- DebuggerRecordNode
--      nodeVal : record val and its operator id
--      childrenList : each list element is a children set that construct the parent node
--                     there could be multiple derivations, so it is a list of children set
data DebuggerRecordNode = DebuggerRecordNode { nodeVal :: DebuggerRecord
                                               , childrenList :: [[DebuggerRecordNode]]
                                               } deriving (Show)


data DebuggerMaps = DebuggerMaps { dbgRecordMap :: DebuggerRecordMap
                                 , dbgRecordWeightMap :: DebuggerRecordWeightMap
                                 } deriving (Show)

emptyDebuggerMaps :: DebuggerMaps
emptyDebuggerMaps = DebuggerMaps M.empty M.empty

-- Return root node of the derivation tree
queryDerivations :: DebuggerRecord -> DebuggerRecordMap -> DebuggerRecordNode
queryDerivations debuggerRecord debuggerRecordMap =
    let root = DebuggerRecordNode { nodeVal = debuggerRecord, childrenList = []}
    in case (dbgOperatorId debuggerRecord) of
        InputRel _ -> root
        InputOp _ -> let derivation = M.lookup debuggerRecord debuggerRecordMap
                           in case derivation of
                               Nothing -> root                   -- control should never arrive here
                               Just derivations -> let childrenList = map (derivationToDebuggerRecordNode debuggerRecordMap) derivations
                                                   in DebuggerRecordNode {nodeVal = debuggerRecord, childrenList = childrenList}

-- helper function used by `queryDerivations`
-- Derivation: [DebuggerRecord], generally has one/two input records, it stands
--             for one possible derivation for a specific output record

derivationToDebuggerRecordNode :: DebuggerRecordMap -> Derivation -> [DebuggerRecordNode]
derivationToDebuggerRecordNode debuggerRecordMap derivation =
    map (\inputrecord -> queryDerivations inputrecord debuggerRecordMap) derivation

handleDebugEvents :: [Event] -> DebuggerMaps -> DatalogProgram -> DebuggerMaps
handleDebugEvents [] dbgMaps _ = dbgMaps
handleDebugEvents (event:events) dbgMaps prog =
    let updatedMaps = handleDebugEvent event dbgMaps prog
    in  handleDebugEvents events updatedMaps prog

handleDebugEvent :: Event -> DebuggerMaps -> DatalogProgram -> DebuggerMaps
handleDebugEvent DebugJoinEvent{..} DebuggerMaps{..} prog =
    let outputRecord = DebuggerRecord { dbgRecord = evtOutput, dbgOperatorId = InputOp evtOperatorId}
        outputRecordWeight = M.lookup outputRecord dbgRecordWeightMap
        updatedWeight = case outputRecordWeight of
                            Nothing -> evtWeight
                            Just w -> evtWeight + w
    in if updatedWeight == 0
       then
            let updatedDbgRecordMap = M.delete outputRecord dbgRecordMap
                updatedDbgRecordWeightMap = M.delete outputRecord dbgRecordWeightMap
            in DebuggerMaps { dbgRecordMap = updatedDbgRecordMap, dbgRecordWeightMap = updatedDbgRecordWeightMap}
       else
            let traceRecords = M.lookup outputRecord dbgRecordMap
                predecessorIds = getPredecessorOpId evtOperatorId prog
                inputRecord1 = DebuggerRecord { dbgRecord = evtInput1, dbgOperatorId = (predecessorIds !! 0)}
                inputRecord2 = DebuggerRecord { dbgRecord = evtInput2, dbgOperatorId = (predecessorIds !! 1)}
                derivation = [inputRecord1, inputRecord2]
                updatedDbgRecordWeightMap = M.insert outputRecord updatedWeight dbgRecordWeightMap
            in case traceRecords of
                Nothing ->  let updatedDbgRecordMap = M.insert outputRecord [derivation] dbgRecordMap
                            in DebuggerMaps { dbgRecordMap = updatedDbgRecordMap, dbgRecordWeightMap = updatedDbgRecordWeightMap}
                Just derivations -> let updatedDbgRecordMap = M.insert outputRecord (derivations ++ [derivation]) dbgRecordMap
                                    in DebuggerMaps { dbgRecordMap = updatedDbgRecordMap, dbgRecordWeightMap = updatedDbgRecordWeightMap}

handleDebugEvent DebugEvent{..} DebuggerMaps{..} prog =
    let outputRecord = DebuggerRecord { dbgRecord = evtOutput, dbgOperatorId = InputOp evtOperatorId}
        outputRecordWeight = M.lookup outputRecord dbgRecordWeightMap
        updatedWeight = case outputRecordWeight of
                            Nothing -> evtWeight
                            Just w -> evtWeight + w
    in if updatedWeight == 0
       then
            let updatedDbgRecordMap = M.delete outputRecord dbgRecordMap
                updatedDbgRecordWeightMap = M.delete outputRecord dbgRecordWeightMap
            in DebuggerMaps { dbgRecordMap = updatedDbgRecordMap, dbgRecordWeightMap = updatedDbgRecordWeightMap}
       else
            let traceRecords = M.lookup outputRecord dbgRecordMap
                predecessorIds = getPredecessorOpId evtOperatorId prog
                inputRecord = DebuggerRecord { dbgRecord = evtInput, dbgOperatorId = (predecessorIds !! 0)}
                derivation = [inputRecord]
                updatedDbgRecordWeightMap = M.insert outputRecord updatedWeight dbgRecordWeightMap
            in case traceRecords of
                Nothing ->  let updatedDbgRecordMap = M.insert outputRecord [derivation] dbgRecordMap
                            in DebuggerMaps { dbgRecordMap = updatedDbgRecordMap, dbgRecordWeightMap = updatedDbgRecordWeightMap}
                Just derivations -> let updatedDbgRecordMap = M.insert outputRecord (derivations ++ [derivation]) dbgRecordMap
                                    in DebuggerMaps { dbgRecordMap = updatedDbgRecordMap, dbgRecordWeightMap = updatedDbgRecordWeightMap}

constructRecordMap :: [Event] -> DebuggerMaps -> DatalogProgram -> DebuggerMaps
constructRecordMap [] recordMaps _ = recordMaps
constructRecordMap (event:events) recordMaps prog =
    let updatedMaps = handleDebugEvent event recordMaps prog
    in constructRecordMap events updatedMaps prog


-- Get the operator if for input records in a debug entry
getPredecessorOpId :: OperatorId -> DatalogProgram-> [OperatorInput]
getPredecessorOpId OperatorId{..} DatalogProgram{..} =
    let Rule{..} = progRules !! ruleIdx
        ruleRhs = ruleRHS !! rhsIdx
    in case ruleRhs of
        RHSLiteral{..} ->
            if rhsIdx == 0
            then [InputRel (atomRelation rhsAtom)]
            else let prevRuleRhs = ruleRHS !! (rhsIdx - 1)
                 in case prevRuleRhs of
                        RHSLiteral{rhsAtom = prevAtom} -> [InputRel (atomRelation prevAtom), InputRel (atomRelation rhsAtom)]
                        _ -> [InputOp OperatorId {ruleIdx = ruleIdx, rhsIdx = (rhsIdx - 1), headIdx = headIdx}, InputRel (atomRelation rhsAtom)]
        RHSCondition{..} -> let prevRhsIdx = getPredecessorRHSRuleIdxForCondition rhsIdx ruleRHS
                        in [InputOp OperatorId {ruleIdx = ruleIdx, rhsIdx = prevRhsIdx, headIdx = headIdx}]
        _ -> [InputOp OperatorId {ruleIdx = ruleIdx, rhsIdx = (rhsIdx - 1), headIdx = headIdx}]

getPredecessorRHSRuleIdxForCondition :: Int -> [RuleRHS] -> Int
getPredecessorRHSRuleIdxForCondition rhsIdx rules =
    case (rules !! (rhsIdx-1)) of
        RHSCondition{..} -> getPredecessorRHSRuleIdxForCondition (rhsIdx-1) rules
        _ -> (rhsIdx-1)
