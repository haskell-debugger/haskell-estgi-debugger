# dap <img src="https://user-images.githubusercontent.com/875324/235317448-1cf2d543-40a8-4eaa-b765-6576ddd7f84f.png" width="5%" />

A [Debug Adaptor Protocol](https://microsoft.github.io/debug-adapter-protocol) (DAP) library.

This library can be used for constructing debug adaptors for any programming language.

# Table of Contents
1. [Usage](#usage)
2. [Build](#build)
3. [Develop](#develop)
4. [Test](#test)
5. [Docs](#docs)

## Usage

For a real-world implementation see the [haskell-estgi-debugger](https://github.com/haskell-debugger/haskell-estgi-debugger).

```haskell
module Main where

import DAP

-- | Runs server in main thread
--
main :: IO ()
main = runDAPServer config mockServerTalk
  where
    capabilities = defaultCapabilities
      { supportsConfigurationDoneRequest      = True
      , supportsHitConditionalBreakpoints     = True
      , supportsModulesRequest                = True
      , additionalModuleColumns               = [ defaultColumnDescriptor
                                                  { columnDescriptorAttributeName = "Extra"
                                                  , columnDescriptorLabel = "Label"
                                                  }
                                                ]
      , supportsValueFormattingOptions        = True
      , supportTerminateDebuggee              = True
      , supportsLoadedSourcesRequest          = True
      }
    config = ServerConfig
      { host = testHost
      , port = testPort
      , serverCapabilities = capabilities
      , debugLogging = False
      }

-- | Mock server communication, used in test runner
--
mockServerTalk
  :: Command
  -> Adaptor app ()
mockServerTalk CommandInitialize = do
  sendInitializeResponse
  sendInitializedEvent
mockServerTalk CommandConfigurationDone = do
  sendConfigurationDoneResponse
  sendStoppedEvent defaultStoppedEvent

-- | Sample port shared amongst client and server
--
testPort :: Int
testPort = 8001

-- | Sample host shared amongst client and server
--
testHost :: String
testHost = "localhost"
```

## Build

```bash
$ cabal build
```

```bash
$ stack build
```

```bash
$ nix build
```

## Develop

```bash
$ nix-shell --run ghcid
```

## Test

```bash
$ cabal test
```

```bash
$ stack test
```

## Docs

```bash
$ stack haddock
```

```bash
$ cabal haddock
```


