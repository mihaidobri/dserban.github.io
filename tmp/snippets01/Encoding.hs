-- Copyright 2007-2009, Judah Jacobson.
-- All Rights Reserved.

-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions are met:

-- - Redistribution of source code must retain the above copyright notice,
-- this list of conditions and the following disclaimer.

-- - Redistribution in binary form must reproduce the above copyright notice,
-- this list of conditions and the following disclaimer in the documentation
-- and/or other materials provided with the distribution.

-- THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS "AS IS" AND ANY
-- EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
-- WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
-- DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR THE CONTRIBUTORS BE LIABLE
-- FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
-- DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
-- SERVICES; LOSS OF USE, DATA OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
-- CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
-- OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE
-- USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
{-# LANGUAGE CPP #-}
module Encoding
    ( encode,decode
    ) where

import Data.ByteString ( ByteString )

#ifdef WIN32
import qualified Encoding.Win32 as Backend ( encode, decode )
#else
import qualified Encoding.IConv as Backend ( encode, decode )
#endif

-- functions redefined to add haddock (there might well be a better way!)

-- | Encode a Unicode 'String' into a 'ByteString' suitable for the current
-- console.
encode :: String -> IO ByteString
encode = Backend.encode

-- | Convert a 'ByteString' from the console's encoding into a Unicode 'String'.
decode :: ByteString -> IO String
decode = Backend.decode
