{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

module With.Assertions (plugin) where

import Control.Monad
import Data.Generics
import Data.Generics.Uniplate.Data
import Data.IORef
import Data.Maybe
import GhcPlugins
import NameCache

import Debug.Trace
import Data.List

plugin :: Plugin
plugin = defaultPlugin
    { installCoreToDos = addSubstituteAssert
    }
  where
    addSubstituteAssert args passes =
      return $ substituteAssertPass : passes

substituteAssertPass :: CoreToDo
substituteAssertPass = CoreDoPluginPass "substitute-assert" substituteAssert

substituteAssert :: ModGuts -> CoreM ModGuts
substituteAssert guts = do
    assert <- assertName
    assertError <- assertErrorVar
    bindsOnlyPass (return . doSubst (fromJust assert) (fromJust assertError)) guts
  where
    doSubst :: Name -> Var -> CoreProgram -> CoreProgram
    doSubst assert assertError = transformBi @CoreProgram @CoreExpr $
      everywhere (mkT $ substExprHead assert assertError)

    substExprHead :: Name -> Var -> CoreExpr -> CoreExpr
    substExprHead assert assertError (Var id) =
       Var $ substVar assert assertError id
    substExprHead _ _ u = u

    substVar :: Name -> Var -> Var -> Var
    substVar assert assertError id =
      if varName id == assert then assertError
      else id

---------------------------------------------
-- finding the real names of our constants --
---------------------------------------------

assertModule :: Module
assertModule = mkModule (stringToUnitId "assert-plugin-0.1.0-MDx86hLmpiH70BRwCmdXN") (mkModuleName "Test.Assert")
-- discovered with
-- >>> trace (showSDocUnsafe $ ppr (map (\(Module u _) -> installedUnitIdFS (toInstalledUnitId u)) (moduleEnvKeys names)))

assertOccName :: OccName
assertOccName = mkVarOcc "assert"

assertErrorOccName :: OccName
assertErrorOccName = mkVarOcc "assertError"

-- getOrigNameCache seems to only get a name cache for local variables. This
-- retrieves the name cache for other modules.
getEnvNameCache :: CoreM OrigNameCache
getEnvNameCache = do
  hsc_env <- getHscEnv
  liftIO $ nsNames <$> readIORef (hsc_NC hsc_env)


assertName :: CoreM (Maybe Name)
assertName = do
  dynFlags <- getDynFlags
  names <- getEnvNameCache
  return $ lookupOrigNameCache names assertModule assertOccName

assertErrorName :: CoreM (Maybe Name)
assertErrorName = do
  names <- getEnvNameCache
  return $ lookupOrigNameCache names assertModule assertErrorOccName

assertErrorVar :: CoreM (Maybe Var)
assertErrorVar = do
  mn <- assertErrorName
  forM mn $ \name ->
    lookupId name
