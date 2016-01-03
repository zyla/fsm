import Data.List

data DesignFile = DesignFile Label Entity Architecture
    deriving (Show)

type Label = String

data Entity = Entity [IfaceSignal]
    deriving (Show)

data IfaceSignal = IfaceSignal Label Mode SignalType
    deriving (Show)

data Mode = In | Out
    deriving (Show)

data RangeTo = RangeTo Int Int
    deriving (Show)

data RangeDownTo = RangeDownTo Int Int
    deriving (Show)

data SignalType = STBit
                | STBitVector RangeDownTo
                | STUnsigned RangeDownTo
                | STSigned RangeDownTo
                | STInteger RangeTo
                | STArray RangeTo SignalType
                | STCustom Label
    deriving (Show)

data Architecture = Architecture [BlockDecl] [ConcurrentStmt]
    deriving (Show)

data BlockDecl = BlockSignal Label SignalType
               | AliasType Label SignalType
               | EnumType Label [Label]
    deriving (Show)

data ConcurrentStmt = AssignStmt Label Expr
                    | ProcessStmt [Label] [SequentialStmt]
    deriving (Show)

-- wasdlfkjsalkdjflsakdjf stahp
data Expr = FCall Label [Expr]
          | Mod Expr Expr
          | PrimLit Literal
    deriving (Show)

type Literal = String

-- dummy
data SequentialStmt = SequentialStmt
    deriving (Show)

ppDesignFile (DesignFile name ent arch) = unlines
    [ "library ieee;"
    , "use ieee.std_logic_1164.all;"
    , "use ieee.numeric_std.all;"
    , ""
    , ppEntity name ent
    , ppArchitecture name arch
    ]

ppEntity name (Entity ifSigs) = unlines
    [ "entity " ++ name ++ " is"
    , "  port ("
    , "    " ++ intercalate ";\n    " (map ppIfaceSignal ifSigs)
    , "  );"
    , "end entity;"
    ]

ppIfaceSignal (IfaceSignal name mode sigType) = concat [ name, ": ", ppMode mode, " ", ppSignalType sigType ]

ppMode In  = "in"
ppMode Out = "out"

ppRangeTo (RangeTo rMin rMax) = concat [ show rMin, " to ", show rMax]

ppRangeDownTo (RangeDownTo rMax rMin) = concat [ show rMax, " downto ", show rMin ]

ppSignalType STBit                 = "std_logic"
ppSignalType (STBitVector rangeDT) = concat [ "std_logic_vector(", ppRangeDownTo rangeDT, ")" ]
ppSignalType (STUnsigned rangeDT)  = concat [ "unsigned(", ppRangeDownTo rangeDT, ")" ]
ppSignalType (STSigned rangeDT)    = concat [ "signed(", ppRangeDownTo rangeDT, ")" ]
ppSignalType (STInteger rangeT)    = concat [ "integer range ", ppRangeTo rangeT ]
ppSignalType (STArray rangeT st)   = concat [ "array (", ppRangeTo rangeT, ") of ", ppSignalType st ]
ppSignalType (STCustom name)       = name

ppArchitecture name (Architecture blockSigs concStmts) = unlines
    [ "architecture " ++ name ++ "_arch is"
    , "  " ++ intercalate ";\n  " (map ppBlockSignal blockSigs) ++ ";"
    , "begin"
    , "end architecture;"
    ]

ppBlockSignal (BlockSignal name sigType) = concat [ "signal ", name, ": ", ppSignalType sigType ]
ppBlockSignal (EnumType name vals)       = concat [ "type ", name, " is (", intercalate ", " vals, ")" ]
ppBlockSignal (AliasType name sigType)   = concat [ "type ", name, " is ", ppSignalType sigType ]

main :: IO ()
main = putStr $ ppDesignFile sineGen
  where
    sineGen = DesignFile
        "sine_gen"
        (Entity
            [ IfaceSignal "clk" In STBit
            , IfaceSignal "dac_scl" Out STBit
            , IfaceSignal "dac_sda" Out STBit
            , IfaceSignal "dac_ldac_n" Out STBit
            , IfaceSignal "dac_rst_n" Out STBit
            , IfaceSignal "dac_sync_n" Out STBit
            ]
        )
        (Architecture
            [ BlockSignal "bit_idx" (STInteger (RangeTo 0 11))
            , BlockSignal "sine_idx" (STInteger (RangeTo 0 49))
            , BlockSignal "sine_val" (STBitVector (RangeDownTo 11 0))
            , EnumType "state_type" ["CMD3", "CMD2", "CMD1", "CMD0", "DATA", "SYNC"]
            , BlockSignal "cur_state" (STCustom "state_type")
            , AliasType "sine_vals_type" (STArray (RangeTo 0 49) (STInteger (RangeTo 0 4095)))
            , BlockSignal "sine_vals" (STCustom "sine_vals_type")
            ]
            []
        )


