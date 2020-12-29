module Path
  ( -- * The basic path type
    Path,
    PathType (..),

    -- * Converting between 'ByteString' and 'Path'
    parseAbs,
    parseRel,
    fromAbs,
    fromRel,

    -- * Operations on paths
    (</>),
    stripPrefix,
    isPrefixOf,
    parent,
    filename,

    -- * Template Haskell stuff
    mkAbs,
    mkRel,
    absp,
    relp,
  )
where

import Path.Internal
