# TODO

## SMTLIB2 format 

See `tests/pos/test0{0,1}.smt2` 

We need the following

```haskell
-- | A datatype to hold the Horn format queries 
--   Language/Fixpoint/Horn/Types.hs 
data HornInfo

-- | A function to parse in .smt2 files
--   Language/Fixpoint/Horn/Parse.hs 
parse :: Text -> HornInfo

-- | A function to translate from Horn into standard FInfo 
--   Language/Fixpoint/Horn/Convert.hs 
convert :: HornInfo -> FInfo 
```


