-----------------------------------------------------------------------------
-- |
-- Module      :  Main
-- Copyright   :  (C) 2023 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <djohnson.m@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
----------------------------------------------------------------------------
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE LambdaCase                 #-}
----------------------------------------------------------------------------
module DAP.Types
  ( -- * Message Type
    MessageType                        (..)
    -- * Types
  , Breakpoint                         (..)
  , Breakpoints                        (..)
  , BreakpointLocation                 (..)
  , Capabilities                       (..)
  , Checksum                           (..)
  , ChecksumAlgorithm                  (..)
  , ColumnDescriptor                   (..)
  , CompletionItem                     (..)
  , CompletionItemType                 (..)
  , DataBreakpoint                     (..)
  , DataBreakpointAccessType           (..)
  , DisassembledInstruction            (..)
  , ExceptionBreakMode                 (..)
  , ExceptionBreakpointsFilter         (..)
  , ExceptionDetails                   (..)
  , ExceptionFilterOptions             (..)
  , ExceptionOptions                   (..)
  , ExceptionPathSegment               (..)
  , FunctionBreakpoint                 (..)
  , GotoTarget                         (..)
  , InstructionBreakpoint              (..)
  , InvalidatedAreas                   (..)
  , Message                            (..)
  , Module                             (..)
  , ModulesViewDescriptor              (..)
  , PresentationHint                   (..)
  , Scope                              (..)
  , Source                             (..)
  , SourceBreakpoint                   (..)
  , SourcePresentationHint             (..)
  , StackFrame                         (..)
  , StackFrameFormat                   (..)
  , StepInTarget                       (..)
  , SteppingGranularity                (..)
  , StoppedEventReason                 (..)
  , Thread                             (..)
  , ThreadEventReason
  , ValueFormat                        (..)
  , Variable                           (..)
  , VariablePresentationHint           (..)
  , ColumnDescriptorType               (..)
  , ScopePresentationHint              (..)
  , PresentationHintKind               (..)
  , PresentationHintAttributes         (..)
  , PresentationHintVisibility         (..)
  , EventGroup                         (..)
  , EventReason                        (..)
  , StartMethod                        (..)
  , EvaluateArgumentsContext           (..)
  , PathFormat                         (..)
  , StackFrame                         (..)
  -- * Command
  , Command                            (..)
  -- * Event
  , EventType                          (..)
  -- ** Events
  , StoppedEvent                       (..)
  , ContinuedEvent                     (..)
  , ExitedEvent                        (..)
  , TerminatedEvent                    (..)
  , ThreadEvent                        (..)
  , OutputEvent                        (..)
  , BreakpointEvent                    (..)
  , ModuleEvent                        (..)
  , LoadedSourceEvent                  (..)
  , ProcessEvent                       (..)
  , CapabilitiesEvent                  (..)
  , ProgressStartEvent                 (..)
  , ProgressUpdateEvent                (..)
  , ProgressEndEvent                   (..)
  , InvalidatedEvent                   (..)
  , MemoryEvent                        (..)
  -- * Server
  , ServerConfig                       (..)
  -- * Client
  , AdaptorClient                      (..)
  , AdaptorState                       (..)
  , AppStore                           (..)
  -- * Errors
  , AdaptorException                   (..)
  , ErrorMessage                       (..)
  , ErrorResponse                      (..)
  -- * Request
  , Request                            (..)
  -- * Misc.
  , PayloadSize
  , Seq
  , SessionId
  -- * Responses
  , CompletionsResponse                (..)
  , ContinueResponse                   (..)
  , DataBreakpointInfoResponse         (..)
  , DisassembleResponse                (..)
  , EvaluateResponse                   (..)
  , ExceptionInfoResponse              (..)
  , GotoTargetsResponse                (..)
  , LoadedSourcesResponse              (..)
  , ModulesResponse                    (..)
  , ReadMemoryResponse                 (..)
  , ScopesResponse                     (..)
  , SetExpressionResponse              (..)
  , SetVariableResponse                (..)
  , SourceResponse                     (..)
  , StackTraceResponse                 (..)
  , StepInTargetsResponse              (..)
  , ThreadsResponse                    (..)
  , VariablesResponse                  (..)
  , WriteMemoryResponse                (..)
  -- * Arguments
  , AttachRequestArguments             (..)
  , BreakpointLocationsArguments       (..)
  , CompletionsArguments               (..)
  , ConfigurationDoneArguments         (..)
  , ContinueArguments                  (..)
  , DataBreakpointInfoArguments        (..)
  , DisassembleArguments               (..)
  , DisconnectArguments                (..)
  , EvaluateArguments                  (..)
  , ExceptionInfoArguments             (..)
  , GotoArguments                      (..)
  , GotoTargetsArguments               (..)
  , InitializeRequestArguments         (..)
  , LaunchRequestArguments             (..)
  , LoadedSourcesArguments             (..)
  , ModulesArguments                   (..)
  , NextArguments                      (..)
  , PauseArguments                     (..)
  , ReadMemoryArguments                (..)
  , RestartArguments                   (..)
  , RestartFrameArguments              (..)
  , ReverseContinueArguments           (..)
  , ScopesArguments                    (..)
  , SetBreakpointsArguments            (..)
  , SetDataBreakpointsArguments        (..)
  , SetExceptionBreakpointsArguments   (..)
  , SetExpressionArguments             (..)
  , SetFunctionBreakpointsArguments    (..)
  , SetInstructionBreakpointsArguments (..)
  , SetVariableArguments               (..)
  , SourceArguments                    (..)
  , StackTraceArguments                (..)
  , StepBackArguments                  (..)
  , StepInArguments                    (..)
  , StepInTargetsArguments             (..)
  , StepOutArguments                   (..)
  , TerminateArguments                 (..)
  , TerminateThreadsArguments          (..)
  , ThreadsArguments                   (..)
  , VariablesArguments                 (..)
  , WriteMemoryArguments               (..)
  , RunInTerminalResponse              (..)
  -- * defaults
  , defaultCapabilities
  -- * Log level
  , Level (..)
  , DebugStatus (..)
  ) where
----------------------------------------------------------------------------
import           Control.Monad.Base              ( MonadBase )
import           Control.Monad.Trans.Control     ( MonadBaseControl )
import           Control.Concurrent              ( ThreadId )
import           Control.Concurrent.MVar         ( MVar )
import           Control.Applicative             ( (<|>) )
import           Data.Typeable                   ( typeRep )
import           Control.Concurrent.STM          ( TVar, newTVarIO )
import           Control.Exception               ( Exception )
import           Control.Monad.State             ( StateT, MonadState, MonadIO )
import           Data.Aeson                      ( (.:), (.:?), withObject, withText, object
                                                 , FromJSON(parseJSON), Value(Null), KeyValue((.=))
                                                 , ToJSON(toJSON), fieldLabelModifier
                                                 , genericParseJSON, genericToJSON, defaultOptions
                                                 )
import           Data.Aeson.Types                ( Pair, typeMismatch )
import           Data.IORef                      ( IORef )
import           Data.Proxy                      ( Proxy(Proxy) )
import           Data.String                     ( IsString(..) )
import           Data.Time                       ( UTCTime )
import           GHC.Generics                    ( Generic )
import           Network.Socket                  ( SockAddr )
import           System.IO                       ( Handle )
import           Text.Read                       ( readMaybe )
import           Data.Text                       (Text)
import qualified Data.Text                       as T ( pack, unpack )
import qualified Data.HashMap.Strict             as H
import           GHC.TypeLits                    (TypeError)
import qualified GHC.TypeLits                    as TypeLits
----------------------------------------------------------------------------
import           DAP.Utils                       ( capitalize, enumToLowerCamel, toLowerCase, modifier, getName, genericParseJSONWithModifier, genericToJSONWithModifier )
----------------------------------------------------------------------------
-- | Core type for Debug Adaptor to send and receive messages in a type safe way.
-- the state is 'AdaptorState' which holds configuration information, along with
-- the current event / response being constructed and the type of the message.
-- Of note: A 'StateT' is used because 'adaptorPayload' should not be shared
-- with other threads.
newtype AdaptorClient store a = AdaptorClient (StateT (AdaptorState store) IO a)
  deriving newtype
    ( Monad
    , MonadIO, Applicative, Functor, MonadState (AdaptorState store)
    , MonadBaseControl IO
    , MonadBase IO
    )
----------------------------------------------------------------------------
-- | The adaptor state is local to a single connection / thread
data AdaptorState app
  = AdaptorState
  { adaptorMessageType  :: MessageType
    -- ^ Current message type being created
    -- This was added as a convenience so we can set the 'request_seq'
    -- and 'command' fields automatically.
    --
  , adaptorPayload      :: ![Pair]
    -- ^ Payload of the current message to be sent
    -- This should never be manually modified by the end user
    -- The payload is accumulated automatically by usage of the API
    --
  , adaptorAppStore     :: AppStore app
    -- ^ Global app store, accessible on a per session basis
    -- Initialized during 'attach' sessions
    --
  , adaptorServerConfig :: ServerConfig
    -- ^ Configuration information for the ServerConfig
    -- Identical across all debugging sessions
    --
  , seqRef              :: IORef Seq
    -- ^ Thread local sequence number, updating as responses and events are set
    --
  , handle              :: Handle
    -- ^ Connection Handle
    --
  , request             :: Request
    -- ^ Connection Request information
    --
  , address             :: SockAddr
    -- ^ Address of Connection
    --
  , sessionId           :: IORef (Maybe SessionId)
    -- ^ Session ID
    -- Local to the current connection's debugger session
    --
  , handleLock          :: MVar ()
    -- ^ A lock for writing to a Handle exists for each new connection
    --
  }
----------------------------------------------------------------------------
type SessionId = Text
----------------------------------------------------------------------------
-- | Used to store a map of debugging sessions
-- The 'ThreadId' is meant to be an asynchronous operation that
-- allows initalized debuggers to emit custom events
-- when they receive messages from the debugger
type AppStore app = TVar (H.HashMap SessionId (ThreadId, app))
----------------------------------------------------------------------------
data ServerConfig
  = ServerConfig
  { host               :: String
  , port               :: Int
  , serverCapabilities :: Capabilities
  , debugLogging       :: Bool
  } deriving stock (Show, Eq)
----------------------------------------------------------------------------
-- | Used to signify a malformed message has been received
data AdaptorException
  = ParseException String
  | ExpectedArguments String
  | DebugSessionIdException String
  | DebuggerException String
  deriving stock (Show, Eq)
  deriving anyclass Exception
----------------------------------------------------------------------------
type PayloadSize = Int
----------------------------------------------------------------------------
data MessageType
  = MessageTypeEvent
  | MessageTypeResponse
  | MessageTypeRequest
  deriving stock (Show, Eq, Generic)
----------------------------------------------------------------------------
instance ToJSON MessageType where
  toJSON = genericToJSONWithModifier
----------------------------------------------------------------------------
type Seq = Int
----------------------------------------------------------------------------
data Request
  = Request
  { args :: Maybe Value
    -- ^ Request arguments
    --
  , requestSeqNum :: Seq
    -- ^ Request sequence number
    --
  , command :: Command
    -- ^ Command of Request
    --
  } deriving stock (Show)
----------------------------------------------------------------------------
instance FromJSON Request where
  parseJSON = withObject "Request" $ \o -> do
    Request
      <$> o .:? "arguments"
      <*> o .: "seq"
      <*> o .: "command"
----------------------------------------------------------------------------
data Breakpoint
  = Breakpoint
  { breakpointId :: Maybe Int
    -- ^
    -- The identifier for the breakpoint. It is needed if breakpoint events are
    -- used to update or remove breakpoints.
    --
  , breakpointVerified :: Bool
    -- ^
    -- If true, the breakpoint could be set (but not necessarily at the desired
    -- location).
    --
  , breakpointMessage :: Maybe Text
    -- ^
    -- A message about the state of the breakpoint.
    -- This is shown to the user and can be used to explain why a breakpoint could
    -- not be verified.
    --
  , breakpointSource :: Maybe Source
    -- ^
    -- The source where the breakpoint is located.
    --
  , breakpointLine :: Maybe Int
    -- ^
    -- The start line of the actual range covered by the breakpoint.
    --
  , breakpointColumn :: Maybe Int
    -- ^
    -- Start position of the source range covered by the breakpoint. It is
    -- measured in UTF-16 code units and the client capability `columnsStartAt1`
    -- determines whether it is 0- or 1-based.
    --
  , breakpointEndLine :: Maybe Int
    -- ^
    -- The end line of the actual range covered by the breakpoint.
    --
  , breakpointEndColumn :: Maybe Int
    -- ^
    -- End position of the source range covered by the breakpoint. It is measured
    -- in UTF-16 code units and the client capability `columnsStartAt1` determines
    -- whether it is 0- or 1-based.
    -- If no end line is given, then the end column is assumed to be in the start
    -- line.
    --
  , breakpointInstructionReference :: Text
    -- ^
    -- A memory reference to where the breakpoint is set.
    --
  , breakpointOffset :: Maybe Int
    -- ^
    -- The offset from the instruction reference.
    -- This can be negative.
    --
  } deriving stock (Show, Eq, Generic)
----------------------------------------------------------------------------
instance ToJSON Breakpoint where
  toJSON = genericToJSONWithModifier
----------------------------------------------------------------------------
newtype Breakpoints breakpoint = Breakpoints [breakpoint]
  deriving stock (Show, Eq)
----------------------------------------------------------------------------
instance ToJSON breakpoint => ToJSON (Breakpoints breakpoint) where
  toJSON (Breakpoints breakpoints)
    = object
    [ "breakpoints" .= breakpoints
    ]
----------------------------------------------------------------------------
data Source
  = Source
  { sourceName :: Maybe Text
    -- ^
    -- The short name of the source. Every source returned from the debug adapter
    -- has a name.
    -- When sending a source to the debug adapter this name is optional.
    --
  , sourcePath :: Maybe Text
    -- ^
    -- The path of the source to be shown in the UI.
    -- It is only used to locate and load the content of the source if no
    -- `sourceReference` is specified (or its value is 0).
    --
  , sourceSourceReference :: Maybe Int
    -- ^
    -- If the value > 0 the contents of the source must be retrieved through the
    -- `source` request (even if a path is specified).
    -- Since a `sourceReference` is only valid for a session, it can not be used
    -- to persist a source.
    -- The value should be less than or equal to 2147483647 (2^31-1).
    --
  , sourcePresentationHint :: Maybe SourcePresentationHint
    -- ^
    -- A hint for how to present the source in the UI.
    -- A value of `deemphasize` can be used to indicate that the source is not
    -- available or that it is skipped on stepping.
    -- Values: 'normal', 'emphasize', 'deemphasize'
    --
  , sourceOrigin :: Maybe Text
    -- ^
    -- The origin of this source. For example, 'internal module', 'inlined content
    -- from source map', etc.
    --
  , sourceSources :: Maybe [Source]
    -- ^
    -- A list of sources that are related to this source. These may be the source
    -- that generated this source.
    --
  , sourceAdapterData :: Maybe Value
    -- ^
    -- Additional data that a debug adapter might want to loop through the client.
    -- The client should leave the data intact and persist it across sessions. The
    -- client should not interpret the data.
    --
  , sourceChecksums :: Maybe [Checksum]
    -- ^
    -- The checksums associated with this file.
    --
  } deriving stock (Show, Eq, Generic)
----------------------------------------------------------------------------
instance FromJSON Source where
   parseJSON = genericParseJSONWithModifier
----------------------------------------------------------------------------
instance ToJSON Source where
  toJSON = genericToJSONWithModifier
----------------------------------------------------------------------------
newtype Sources = Sources { getSources :: [Source] } deriving stock (Show, Eq)
----------------------------------------------------------------------------
instance ToJSON Sources where toJSON (Sources s) = object [ "sources" .= s ]
----------------------------------------------------------------------------
data SourcePresentationHint
  = SourcePresentationHintNormal
  | SourcePresentationHintEmphasize
  | SourcePresentationHintDeemphasize
  deriving stock (Show, Eq, Generic)
----------------------------------------------------------------------------
instance FromJSON SourcePresentationHint where
   parseJSON = genericParseJSONWithModifier
----------------------------------------------------------------------------
instance ToJSON SourcePresentationHint where
  toJSON = genericToJSONWithModifier
----------------------------------------------------------------------------
data PresentationHint
  = PresentationHintNormal
  | PresentationHintLabel
  | PresentationHintSubtle
  deriving stock (Show, Eq, Generic)
----------------------------------------------------------------------------
instance ToJSON PresentationHint where
  toJSON = genericToJSONWithModifier
----------------------------------------------------------------------------
data Checksum
  = Checksum
  { algorithm :: ChecksumAlgorithm
    -- ^ The algorithm used to calculate this checksum.
    --
  , checksum :: Text
    -- ^ Value of the checksum, encoded as a hexadecimal value.
    --
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (ToJSON, FromJSON)
----------------------------------------------------------------------------
data ChecksumAlgorithm
  = MD5
  | SHA1
  | SHA256
  | TimeStamp UTCTime
  deriving stock (Show, Eq)
----------------------------------------------------------------------------
instance ToJSON ChecksumAlgorithm where
  toJSON MD5                 = "md5"
  toJSON SHA1                = "sha1"
  toJSON SHA256              = "sha256"
  toJSON (TimeStamp utcTime) = toJSON utcTime
----------------------------------------------------------------------------
instance FromJSON ChecksumAlgorithm where
  parseJSON = withText name $ \txt ->
    case txt of
      "md5" -> pure MD5
      "sha1" -> pure SHA1
      "sha256" -> pure SHA256
      s -> typeMismatch name (toJSON s)
    where
      name = getName (Proxy @ChecksumAlgorithm)
----------------------------------------------------------------------------
data StackFrame
  = StackFrame
  { stackFrameId :: Int
    -- ^
    -- An identifier for the stack frame. It must be unique across all threads.
    -- This id can be used to retrieve the scopes of the frame with the `scopes`
    -- request or to restart the execution of a stack frame.
    --
  , stackFrameName :: Text
    -- ^
    -- The name of the stack frame, typically a method name.
    --
  , stackFrameSource :: Maybe Source
    -- ^
    -- The source of the frame.
    --
  , stackFrameLine :: Int
    -- ^
    -- The line within the source of the frame. If the source attribute is missing
    -- or doesn't exist, `line` is 0 and should be ignored by the client.
    --
  , stackFrameColumn :: Int
    -- ^
    -- Start position of the range covered by the stack frame. It is measured in
    -- UTF-16 code units and the client capability `columnsStartAt1` determines
    -- whether it is 0- or 1-based. If attribute `source` is missing or doesn't
    -- exist, `column` is 0 and should be ignored by the client.
    --
  , stackFrameEndLine :: Int
    -- ^
    -- The end line of the range covered by the stack frame.
    --
  , stackFrameEndColumn :: Int
    -- ^
    -- End position of the range covered by the stack frame. It is measured in
    -- UTF-16 code units and the client capability `columnsStartAt1` determines
    -- whether it is 0- or 1-based.
    --
  , stackFrameCanRestart :: Bool
    -- ^
    -- Indicates whether this frame can be restarted with the `restart` request.
    -- Clients should only use this if the debug adapter supports the `restart`
    -- request and the corresponding capability `supportsRestartRequest` is true.
    -- If a debug adapter has this capability, then `canRestart` defaults to
    -- `true` if the property is absent.
    --
  , stackFrameInstructionPointerReference :: Maybe Text
    -- ^
    -- A memory reference for the current instruction pointer in this frame.
    --
  , stackFrameModuleId :: Maybe (Either Int Text)
    -- ^
    -- The module associated with this frame, if any.
    --
  , stackFramePresentationHint :: Maybe PresentationHint
    -- ^
    -- A hint for how to present this frame in the UI.
    -- A value of `label` can be used to indicate that the frame is an artificial
    -- frame that is used as a visual label or separator. A value of `subtle` can
    -- be used to change the appearance of a frame in a 'subtle' way.
    -- Values: 'normal', 'label', 'subtle'
    --
  } deriving stock (Show, Eq)
----------------------------------------------------------------------------
instance ToJSON StackFrame where
  toJSON StackFrame {..}
    = object
    [ "id"                          .= stackFrameId
    , "name"                        .= stackFrameName
    , "source"                      .= stackFrameSource
    , "line"                        .= stackFrameLine
    , "column"                      .= stackFrameColumn
    , "endLine"                     .= stackFrameEndLine
    , "endColumn"                   .= stackFrameEndColumn
    , "canRestart"                  .= stackFrameCanRestart
    , "instructionPointerReference" .= stackFrameInstructionPointerReference
    , "moduleId"                    .= modId
    , "presentationHint"            .= stackFramePresentationHint
    ] where
        modId = maybe Null (either toJSON toJSON) stackFrameModuleId
----------------------------------------------------------------------------
data Thread
  = Thread
  { threadId :: Int
    -- ^ Unique identifier for the thread.
    --
  , threadName :: Text
    -- ^ The name of the thread.
    --
  } deriving stock (Show, Eq, Generic)
----------------------------------------------------------------------------
instance ToJSON Thread where
  toJSON = genericToJSONWithModifier
----------------------------------------------------------------------------
instance FromJSON Thread where
  parseJSON = genericParseJSONWithModifier
----------------------------------------------------------------------------
defaultCapabilities :: Capabilities
defaultCapabilities = capabilities
  where
    capabilities = Capabilities
      { supportsConfigurationDoneRequest      = True
      , supportsFunctionBreakpoints           = False
      , supportsConditionalBreakpoints        = False
      , supportsHitConditionalBreakpoints     = False
      , supportsEvaluateForHovers             = False
      , exceptionBreakpointFilters            = []
      , supportsStepBack                      = False
      , supportsSetVariable                   = True
      , supportsRestartFrame                  = False
      , supportsGotoTargetsRequest            = False
      , supportsStepInTargetsRequest          = False
      , supportsCompletionsRequest            = False
      , completionTriggerCharacters           = []
      , supportsModulesRequest                = True
      , additionalModuleColumns               = []
      , supportedChecksumAlgorithms           = []
      , supportsRestartRequest                = False
      , supportsExceptionOptions              = False
      , supportsValueFormattingOptions        = True
      , supportsExceptionInfoRequest          = False
      , supportTerminateDebuggee              = False
      , supportSuspendDebuggee                = False
      , supportsDelayedStackTraceLoading      = False
      , supportsLoadedSourcesRequest          = True
      , supportsLogPoints                     = False
      , supportsTerminateThreadsRequest       = False
      , supportsSetExpression                 = False
      , supportsTerminateRequest              = False
      , supportsDataBreakpoints               = False
      , supportsReadMemoryRequest             = False
      , supportsWriteMemoryRequest            = False
      , supportsDisassembleRequest            = False
      , supportsCancelRequest                 = False
      , supportsBreakpointLocationsRequest    = False
      , supportsClipboardContext              = False
      , supportsSteppingGranularity           = False
      , supportsInstructionBreakpoints        = False
      , supportsExceptionFilterOptions        = False
      , supportsSingleThreadExecutionRequests = False
      }
----------------------------------------------------------------------------
data Capabilities
  = Capabilities
  { supportsConfigurationDoneRequest :: Bool
    -- ^ The debug adapter supports the `configurationDone` request.
    --
  , supportsFunctionBreakpoints :: Bool
    -- ^ The debug adapter supports function breakpoints.
    --
  , supportsConditionalBreakpoints :: Bool
    -- ^ The debug adapter supports conditional breakpoints.
    --
  , supportsHitConditionalBreakpoints :: Bool
    -- ^ The debug adapter supports breakpoints that break execution after a
    -- specified number of hits.
    --
  , supportsEvaluateForHovers :: Bool
    -- ^ The debug adapter supports a (side effect free) `evaluate` request for data
    -- hovers.
    --
  , exceptionBreakpointFilters :: [ExceptionBreakpointsFilter]
    -- ^ Available exception filter options for the `setExceptionBreakpoints`
    -- ^ request.
    --
  , supportsStepBack :: Bool
    -- ^ The debug adapter supports stepping back via the `stepBack` and
    -- ^ `reverseContinue` requests.
    --
  , supportsSetVariable :: Bool
    -- ^ The debug adapter supports setting a variable to a value.
    --
  , supportsRestartFrame :: Bool
    -- ^ The debug adapter supports restarting a frame.
    --
  , supportsGotoTargetsRequest :: Bool
    -- ^ The debug adapter supports the `gotoTargets` request.
    --
  , supportsStepInTargetsRequest :: Bool
    -- ^ The debug adapter supports the `stepInTargets` request.
    --
  , supportsCompletionsRequest :: Bool
    -- ^ The debug adapter supports the `completions` request.
    --
  , completionTriggerCharacters :: [Text]
    -- ^ The set of characters that should trigger completion in a REPL. If not
    -- ^ specified, the UI should assume the `.` character.
    --
  , supportsModulesRequest :: Bool
    -- ^ The debug adapter supports the `modules` request.
    --
  , additionalModuleColumns :: [ColumnDescriptor]
    -- ^ The set of additional module information exposed by the debug adapter.
    --
  , supportedChecksumAlgorithms :: [ChecksumAlgorithm]
    -- ^ Checksum algorithms supported by the debug adapter.
    --
  , supportsRestartRequest :: Bool
    -- ^ The debug adapter , supports the `restart` request. In this case a client
    -- ^ should not implement `restart` by terminating and relaunching the adapter
    -- ^ but by calling the `restart` request.
    --
  , supportsExceptionOptions :: Bool
    -- ^ The debug adapter , supports `exceptionOptions` on the
    -- ^ `setExceptionBreakpoints` request.
    --
  , supportsValueFormattingOptions :: Bool
    -- ^ The debug adapter , supports a `format` attribute on the `stackTrace`,
    -- ^ `variables`, and `evaluate` requests.
    --
  , supportsExceptionInfoRequest :: Bool
    -- ^ The debug adapter , supports the `exceptionInfo` request.
    --
  , supportTerminateDebuggee :: Bool
    -- ^ The debug adapter , supports the `terminateDebuggee` attribute on the `disconnect` request.
    --
  , supportSuspendDebuggee :: Bool
    -- ^ The debug adapter , supports the `suspendDebuggee` attribute on the `disconnect` request.
    --
  , supportsDelayedStackTraceLoading :: Bool
    -- ^ The debug adapter , supports the delayed loading of parts of the stack, which
    -- ^ requires that both the `startFrame` and `levels` arguments and the
    -- ^ `totalFrames` result of the `stackTrace` request are , supported.
    --
  , supportsLoadedSourcesRequest :: Bool
    -- ^ The debug adapter , supports the `loadedSources` request.
    --
  , supportsLogPoints :: Bool
    -- ^ The debug adapter , supports log points by interpreting the `logMessage`
    -- ^ attribute of the `SourceBreakpoint`.
    --
  , supportsTerminateThreadsRequest :: Bool
    -- ^ The debug adapter , supports the `terminateThreads` request.
    --
  , supportsSetExpression :: Bool
    -- ^ The debug adapter , supports the `setExpression` request.
    --
  , supportsTerminateRequest :: Bool
    -- ^ The debug adapter , supports the `terminate` request.
    --
  , supportsDataBreakpoints :: Bool
    -- ^ The debug adapter , supports data breakpoints.
    --
  , supportsReadMemoryRequest :: Bool
    -- ^ The debug adapter , supports the `readMemory` request.
    --
  , supportsWriteMemoryRequest :: Bool
    -- ^ The debug adapter , supports the `writeMemory` request.
    --
  , supportsDisassembleRequest :: Bool
    -- ^ The debug adapter , supports the `disassemble` request.
    --
  , supportsCancelRequest :: Bool
    -- ^ The debug adapter , supports the `cancel` request.
    --
  , supportsBreakpointLocationsRequest :: Bool
    -- ^ The debug adapter , supports the `breakpointLocations` request.
    --
  , supportsClipboardContext :: Bool
    -- ^ The debug adapter , supports the `clipboard` context value in the `evaluate`
    -- request.
    --
  , supportsSteppingGranularity :: Bool
    -- ^ The debug adapter , supports stepping granularities (argument `granularity`)
    -- for the stepping requests.
    --
  , supportsInstructionBreakpoints :: Bool
    -- ^ The debug adapter , supports adding breakpoints based on instruction
    -- references.
    --
  , supportsExceptionFilterOptions :: Bool
    -- ^ The debug adapter , supports `filterOptions` as an argument on the
    -- `setExceptionBreakpoints` request.
    --
  , supportsSingleThreadExecutionRequests :: Bool
    -- ^ The debug adapter , supports the `singleThread` property on the execution
    -- requests (`continue`, `next`, `stepIn`, `stepOut`, `reverseContinue`,
    -- `stepBack`).
    --
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (ToJSON)
----------------------------------------------------------------------------
data EventType
  = EventTypeInitialized
  | EventTypeStopped
  | EventTypeContinued
  | EventTypeExited
  | EventTypeTerminated
  | EventTypeThread
  | EventTypeOutput
  | EventTypeBreakpoint
  | EventTypeModule
  | EventTypeLoadedSource
  | EventTypeProcess
  | EventTypeCapabilities
  | EventTypeProgressStart
  | EventTypeProgressUpdate
  | EventTypeProgressEnd
  | EventTypeInvalidated
  | EventTypeMemory
  deriving stock (Show, Eq, Read, Generic)
----------------------------------------------------------------------------
instance ToJSON EventType where
  toJSON = genericToJSONWithModifier
----------------------------------------------------------------------------
data Command
  = CommandCancel
  | CommandRunInTerminal
  | CommandStartDebugging
  | CommandInitialize
  | CommandConfigurationDone
  | CommandLaunch
  | CommandAttach
  | CommandRestart
  | CommandDisconnect
  | CommandTerminate
  | CommandBreakpointLocations
  | CommandSetBreakpoints
  | CommandSetFunctionBreakpoints
  | CommandSetExceptionBreakpoints
  | CommandDataBreakpointInfo
  | CommandSetDataBreakpoints
  | CommandSetInstructionBreakpoints
  | CommandContinue
  | CommandNext
  | CommandStepIn
  | CommandStepOut
  | CommandStepBack
  | CommandReverseContinue
  | CommandRestartFrame
  | CommandGoTo
  | CommandPause
  | CommandStackTrace
  | CommandScopes
  | CommandVariables
  | CommandSetVariable
  | CommandSource
  | CommandThreads
  | CommandTerminateThreads
  | CommandModules
  | CommandLoadedSources
  | CommandEvaluate
  | CommandSetExpression
  | CommandStepInTargets
  | CommandGoToTargets
  | CommandCompletions
  | CommandExceptionInfo
  | CommandReadMemory
  | CommandWriteMemory
  | CommandDisassemble
  | CommandUnknown Text
  deriving stock (Show, Eq, Read, Generic)
----------------------------------------------------------------------------
instance FromJSON Command where
  parseJSON = withText name $ \command ->
    case readMaybe (name <> capitalize (T.unpack command)) of
      Just cmd ->
        pure cmd
      Nothing ->
        pure (CommandUnknown command)
    where
      name = show (typeRep (Proxy @Command))
----------------------------------------------------------------------------
instance ToJSON Command where
  toJSON (CommandUnknown x) = toJSON x
  toJSON cmd = genericToJSONWithModifier cmd
----------------------------------------------------------------------------
data ErrorMessage
  = ErrorMessageCancelled
  | ErrorMessageNotStopped
  | ErrorMessageCustom Text
  deriving stock (Show, Eq, Generic)
----------------------------------------------------------------------------
instance IsString ErrorMessage where
  fromString = ErrorMessageCustom . T.pack
----------------------------------------------------------------------------
instance ToJSON ErrorMessage where
  toJSON (ErrorMessageCustom e) = toJSON e
  toJSON msg = genericToJSONWithModifier msg
----------------------------------------------------------------------------
data BreakpointLocation
  = BreakpointLocation
  { breakpointLocationLine :: Int
    -- ^
    -- Start line of breakpoint location.
    --
  , breakpointLocationColumn :: Maybe Int
    -- ^
    -- The start position of a breakpoint location. Position is measured in UTF-16
    -- code units and the client capability `columnsStartAt1` determines whether
    -- it is 0- or 1-based.
    --
  , breakpointLocationEndLine :: Maybe Int
    -- ^
    -- The end line of breakpoint location if the location covers a range.
    --
  , breakpointLocationEndColumn :: Maybe Int
    -- ^
    -- The end position of a breakpoint location (if the location covers a range).
    -- Position is measured in UTF-16 code units and the client capability
    -- `columnsStartAt1` determines whether it is 0- or 1-based.
    --
  } deriving stock (Eq, Show)
----------------------------------------------------------------------------
instance ToJSON BreakpointLocation where
  toJSON BreakpointLocation {..}
    = object
    [ "line"      .= breakpointLocationLine
    , "column"    .= breakpointLocationColumn
    , "endLine"   .= breakpointLocationEndLine
    , "endColumn" .= breakpointLocationEndLine
    ]
----------------------------------------------------------------------------
data ContinueResponse
  = ContinueResponse
  { continueResponseAllThreadsContinued :: Bool
    -- ^
    -- The value true (or a missing property) signals to the client that all
    -- threads have been resumed. The value false indicates that not all threads
    -- were resumed.
    --
  } deriving stock (Show, Eq)
----------------------------------------------------------------------------
instance ToJSON ContinueResponse where
  toJSON ContinueResponse {..}
    = object
    [ "allThreadsContinued" .= continueResponseAllThreadsContinued
    ]
----------------------------------------------------------------------------
-- | On error (whenever success is false), the body can provide more details.
newtype ErrorResponse
  = ErrorResponse
  { errorResponseError :: Maybe Message
  } deriving stock (Show, Eq)
----------------------------------------------------------------------------
instance ToJSON ErrorResponse where
  toJSON ErrorResponse {..} = object [ "error" .= errorResponseError ]
----------------------------------------------------------------------------
data Message
  = Message
  { messageId :: Int
    -- ^
    -- Unique (within a debug adapter implementation) identifier for the message.
    -- The purpose of these error IDs is to help extension authors that have the
    -- requirement that every user visible error message needs a corresponding
    -- error number, so that users or customer support can find information about
    -- the specific error more easily.
    --
  , messageFormat :: Text
    -- ^
    -- A format string for the message. Embedded variables have the form `{name}`.
    -- If variable name starts with an underscore character, the variable does not
    -- contain user data (PII) and can be safely used for telemetry purposes.
    --
  , messageVariables :: Maybe (H.HashMap Text Text)
    -- ^
    -- An object used as a dictionary for looking up the variables in the format
    -- string.
    --
  , messageSendTelemetry :: Bool
    -- ^
    -- If true send to telemetry.
    --
  , messageShowUser :: Maybe Bool
    -- ^
    -- If true show user.
    --
  , messageUrl :: Maybe Text
    -- ^
    -- A url where additional information about this message can be found.
    --
  , messageUrlLabel :: Maybe Text
    -- ^
    -- A label that is presented to the user as the UI for opening the url.
    --
  } deriving stock (Show, Eq)
----------------------------------------------------------------------------
instance ToJSON Message where
  toJSON Message {..} =
    object
    [ "id"            .= messageId
    , "format"        .= messageFormat
    , "variables"     .= messageVariables
    , "sendTelemetry" .= messageSendTelemetry
    , "showUser"      .= messageShowUser
    , "url"           .= messageUrl
    , "urlLabel"      .= messageUrlLabel
    ]
----------------------------------------------------------------------------
data RunInTerminalResponse
  = RunInTerminalResponse
  { runInTerminalResponseProcessId :: Maybe Int
    -- ^
    -- The process ID. The value should be less than or equal to 2147483647
    -- (2^31-1).
    --
  , runInTerminalResponseShellProcessId :: Maybe Int
    -- ^
    -- The process ID of the terminal shell. The value should be less than or
    -- equal to 2147483647 (2^31-1).
    --
  } deriving stock (Show, Eq)
----------------------------------------------------------------------------
instance ToJSON RunInTerminalResponse where
  toJSON RunInTerminalResponse {..} =
    object
    [ "processId"      .= runInTerminalResponseProcessId
    , "shellProcessId" .= runInTerminalResponseShellProcessId
    ]
----------------------------------------------------------------------------
data ModulesResponse
  = ModulesResponse
  { modules :: [Module]
    -- ^
    -- All modules or range of modules.
    --
  , totalModules :: Maybe Int
    -- ^
    -- The total number of modules available.
    --
  } deriving stock (Show, Eq)
----------------------------------------------------------------------------
instance ToJSON ModulesResponse where
  toJSON ModulesResponse {..}
    = object
    [ "modules"      .= modules
    , "totalModules" .= totalModules
    ]
----------------------------------------------------------------------------
data Module
  = Module
  { moduleId :: Either Text Int
    -- ^
    -- Unique identifier for the module.
    --
  , moduleName :: Text
    -- ^
    -- A name of the module.
    --
  , modulePath :: Text
    -- ^
    -- Logical full path to the module. The exact definition is implementation
    -- defined, but usually this would be a full path to the on-disk file for the
    -- module.
    --
  , moduleIsOptimized :: Maybe Bool
    -- ^
    -- True if the module is optimized.
    --
  , moduleIsUserCode :: Maybe Bool
    -- ^
    -- True if the module is considered 'user code' by a debugger that supports
    -- 'Just My Code'.
    --
  , moduleVersion :: Maybe Text
    -- ^
    -- Version of Module.
    --
  , moduleSymbolStatus :: Maybe Text
    -- ^
    -- User-understandable description of if symbols were found for the module
    -- (ex: 'Symbols Loaded', 'Symbols not found', etc.)
    --
  , moduleSymbolFilePath :: Maybe Text
    -- ^
    -- Logical full path to the symbol file. The exact definition is
    -- implementation defined.
    --
  , moduleDateTimeStamp :: Maybe Text
    -- ^
    -- Module created or modified, encoded as a RFC 3339 timestamp.
    --
  , moduleAddressRange :: Maybe Text
    -- ^
    -- Address range covered by this module.
    --
  } deriving stock (Show, Eq)
----------------------------------------------------------------------------
instance ToJSON Module where
  toJSON Module {..}
    = object
    [ "id"             .= moduleId
    , "name"           .= moduleName
    , "path"           .= modulePath
    , "isOptimized"    .= moduleIsOptimized
    , "isUserCode"     .= moduleIsUserCode
    , "version"        .= moduleVersion
    , "symbolStatus"   .= moduleSymbolStatus
    , "symbolFilePath" .= moduleSymbolFilePath
    , "dateTimeStamp"  .= moduleDateTimeStamp
    , "addressRange"   .= moduleAddressRange
    ]
----------------------------------------------------------------------------
data DataBreakpointInfoResponse
  = DataBreakpointInfoResponse
  { dataBreakpointInfoResponseDataId :: Maybe Text
    -- ^
    -- An identifier for the data on which a data breakpoint can be registered
    -- with the `setDataBreakpoints` request or null if no data breakpoint is
    -- available.
    --
  , dataBreakpointInfoResponseDescription :: Text
    -- ^
    -- UI string that describes on what data the breakpoint is set on or why a
    -- data breakpoint is not available.
    --
  , dataBreakpointInfoResponseDescriptionAccessTypes :: [DataBreakpointAccessType]
    -- ^
    -- Attribute lists the available access types for a potential data
    -- breakpoint. A UI client could surface this information.
    --
  , dataBreakpointInfoResponseDescriptionCanPersist :: Maybe Bool
    -- ^
    -- Attribute indicates that a potential data breakpoint could be persisted
    -- across sessions.
    --
  } deriving stock (Show, Eq)
----------------------------------------------------------------------------
instance ToJSON DataBreakpointInfoResponse where
  toJSON DataBreakpointInfoResponse {..}
    = object
    [ "dataId"      .= dataBreakpointInfoResponseDataId
    , "description" .= dataBreakpointInfoResponseDescription
    , "accessTypes" .= dataBreakpointInfoResponseDescriptionAccessTypes
    , "canPersist"  .= dataBreakpointInfoResponseDescriptionCanPersist
    ]
----------------------------------------------------------------------------
-- | This enumeration defines all possible access types for data breakpoints.
-- Values: ‘read’, ‘write’, ‘readWrite’
data DataBreakpointAccessType
  = DataBreakpointAccessTypeRead
  | DataBreakpointAccessTypeWrite
  | DataBreakpointAccessTypeReadWrite
  deriving stock (Show, Eq)
----------------------------------------------------------------------------
-- | type DataBreakpointAccessType = 'read' | 'write' | 'readWrite';
instance ToJSON DataBreakpointAccessType where
  toJSON = enumToLowerCamel (Proxy @DataBreakpointAccessType)
----------------------------------------------------------------------------
data StackTraceResponse
  = StackTraceResponse
  { stackFrames :: [StackFrame]
    -- ^
    -- The frames of the stack frame. If the array has length zero, there are no
    -- stack frames available.
    -- This means that there is no location information available.
    --
  , totalFrames :: Maybe Int
    -- ^
    -- The total number of frames available in the stack. If omitted or if
    -- `totalFrames` is larger than the available frames, a client is expected
    -- to request frames until a request returns less frames than requested
    -- (which indicates the end of the stack). Returning monotonically
    -- increasing `totalFrames` values for subsequent requests can be used to
    -- enforce paging in the client.
    --
  } deriving stock (Show, Eq)
----------------------------------------------------------------------------
instance ToJSON StackTraceResponse where
  toJSON StackTraceResponse {..}
    = object
    [ "stackFrames" .= stackFrames
    , "totalFrames" .= totalFrames
    ]
----------------------------------------------------------------------------
newtype ScopesResponse
  = ScopesResponse
  { scopes :: [Scope]
    -- ^
    -- The scopes of the stack frame. If the array has length zero, there are no
    -- scopes available.
  } deriving stock (Show, Eq)
----------------------------------------------------------------------------
instance ToJSON ScopesResponse where
  toJSON ScopesResponse {..}
    = object
    [ "scopes" .= scopes
    ]
----------------------------------------------------------------------------
data Scope
  = Scope
  { scopeName :: Text
    -- ^
    -- Name of the scope such as 'Arguments', 'Locals', or 'Registers'. This
    -- string is shown in the UI as is and can be translated.
    --
  , presentationHint :: Maybe ScopePresentationHint
    -- ^
    -- A hint for how to present this scope in the UI. If this attribute is
    -- missing, the scope is shown with a generic UI.
    -- Values:
    -- 'arguments': Scope contains method arguments.
    -- 'locals': Scope contains local variables.
    -- 'registers': Scope contains registers. Only a single `registers` scope
    -- should be returned from a `scopes` request.
    -- etc.
    --
  , variablesReference :: Int
    -- ^
    -- The variables of this scope can be retrieved by passing the value of
    -- `variablesReference` to the `variables` request as long as execution
    -- remains suspended. See 'Lifetime of Object References' in the Overview
    -- section for details.
    --
  , namedVariables :: Maybe Int
    -- ^
    -- The number of named variables in this scope.
    -- The client can use this information to present the variables in a paged UI
    -- and fetch them in chunks.
    --
  , indexedVariables :: Maybe Int
    -- ^
    -- The number of indexed variables in this scope.
    -- The client can use this information to present the variables in a paged UI
    -- and fetch them in chunks.
    --
  , expensive :: Bool
    -- ^
    -- If true, the number of variables in this scope is large or expensive to
    -- retrieve.
    --
  , source :: Maybe Source
    -- ^
    -- The source for this scope.
    --
  , line :: Maybe Int
    -- ^
    -- The start line of the range covered by this scope.
    --
  , column :: Maybe Int
    -- ^
    -- Start position of the range covered by the scope. It is measured in UTF-16
    -- code units and the client capability `columnsStartAt1` determines whether
    -- it is 0- or 1-based.
    --
  , endLine :: Maybe Int
    -- ^
    -- The end line of the range covered by this scope.
    --
  , endColumn :: Maybe Int
    -- ^
    -- End position of the range covered by the scope. It is measured in UTF-16
    -- code units and the client capability `columnsStartAt1` determines whether
    -- it is 0- or 1-based.
    --
  } deriving stock (Show, Eq)
----------------------------------------------------------------------------
instance ToJSON Scope where
  toJSON Scope {..}
    = object
    [ "name"              .= scopeName
    , "presentationHint"  .= presentationHint
    , "variablesReference" .= variablesReference
    , "namedVariables"    .= namedVariables
    , "indexedVariables"  .= indexedVariables
    , "expensive"         .= expensive
    , "source"            .= source
    , "line"              .= line
    , "column"            .= column
    , "endLine"           .= endLine
    , "endColumn"         .= endColumn
    ]
----------------------------------------------------------------------------
data ScopePresentationHint
  = ScopePresentationHintArguments
  | ScopePresentationHintLocals
  | ScopePresentationHintRegister
  | ScopePresentationHint Text
  deriving stock (Show, Eq)
----------------------------------------------------------------------------
instance ToJSON ScopePresentationHint where
  toJSON ScopePresentationHintArguments = "arguments"
  toJSON ScopePresentationHintLocals    = "locals"
  toJSON ScopePresentationHintRegister  = "register"
  toJSON (ScopePresentationHint hint)   = toJSON hint
----------------------------------------------------------------------------
data VariablesResponse
  = VariablesResponse
  { variables :: [Variable]
    -- ^
    -- All (or a range) of variables for the given variable reference.
    --
  } deriving stock (Show, Eq, Generic)
    deriving anyclass ToJSON
----------------------------------------------------------------------------
data Variable
  = Variable
  { variableName :: Text
    -- ^
    -- The variable's name.
    --
  , variableValue :: Text
    -- ^
    -- The variable's value.
    -- This can be a multi-line text, e.g. for a function the body of a function.
    -- For structured variables (which do not have a simple value), it is
    -- recommended to provide a one-line representation of the structured object.
    -- This helps to identify the structured object in the collapsed state when
    -- its children are not yet visible.
    -- An empty string can be used if no value should be shown in the UI.
    --
  , variableType :: Maybe Text
    -- ^
    -- The type of the variable's value. Typically shown in the UI when hovering
    -- over the value.
    -- This attribute should only be returned by a debug adapter if the
    -- corresponding capability `supportsVariableType` is true.
    --
  , variablePresentationHint :: Maybe VariablePresentationHint
    -- ^
    -- Properties of a variable that can be used to determine how to render the
    -- variable in the UI.
    --
  , variableEvaluateName :: Maybe Text
    -- ^
    -- The evaluatable name of this variable which can be passed to the `evaluate`
    -- request to fetch the variable's value.
    --
  , variableVariablesReference :: Int
    -- ^
    -- If `variablesReference` is > 0, the variable is structured and its children
    -- can be retrieved by passing `variablesReference` to the `variables` request
    -- as long as execution remains suspended. See 'Lifetime of Object References'
    -- in the Overview section for details.
    --
  , variableNamedVariables :: Maybe Int
    -- ^
    -- The number of named child variables.
    -- The client can use this information to present the children in a paged UI
    -- and fetch them in chunks.
    --
  , variableIndexedVariables :: Maybe Int
    -- ^
    -- The number of indexed child variables.
    -- The client can use this information to present the children in a paged UI
    -- and fetch them in chunks.
    --
  , variableMemoryReference :: Maybe Text
    -- ^
    -- The memory reference for the variable if the variable represents executable
    -- code, such as a function pointer.
    -- This attribute is only required if the corresponding capability
    -- `supportsMemoryReferences` is true.
    --
   } deriving stock (Show, Eq, Generic)
----------------------------------------------------------------------------
instance ToJSON Variable where
  toJSON = genericToJSONWithModifier
    -- = object
    -- [ "value"            .= variableValue
    -- , "type"             .= variableType
    -- , "presentationHint" .= variablePresentationHint
    -- , "evaluateName"     .= variableEvaluateName
    -- , "reference"        .= variablesReference
    -- , "namedVariables"   .= variableNamedVariables
    -- , "IndexedVariables" .= variableIndexedVariables
    -- , "memoryReference"  .= variableMemoryReference
    -- ]
----------------------------------------------------------------------------
data VariablePresentationHint
  = VariablePresentationHint
  { variablePresentationHintKind :: PresentationHintKind
    -- ^
    -- The kind of variable. Before introducing additional values, try to use the
    -- listed values.
    -- Values:
    -- 'property': Indicates that the object is a property.
    -- 'method': Indicates that the object is a method.
    -- 'class': Indicates that the object is a class.
    -- 'data': Indicates that the object is data.
    -- 'event': Indicates that the object is an event.
    -- 'baseClass': Indicates that the object is a base class.
    -- 'innerClass': Indicates that the object is an inner class.
    -- 'interface': Indicates that the object is an interface.
    -- 'mostDerivedClass': Indicates that the object is the most derived class.
    -- 'virtual': Indicates that the object is virtual, that means it is a
    -- synthetic object introduced by the adapter for rendering purposes, e.g. an
    -- index range for large arrays.
    -- 'dataBreakpoint': Deprecated: Indicates that a data breakpoint is
    -- registered for the object. The `hasDataBreakpoint` attribute should
    -- generally be used instead.
    -- etc.
    --
  , variablePresentationHintAttributes :: [PresentationHintAttributes]
    -- ^
    -- Set of attributes represented as an array of strings. Before introducing
    -- additional values, try to use the listed values.
    -- Values:
    -- 'static': Indicates that the object is static.
    -- 'constant': Indicates that the object is a constant.
    -- 'readOnly': Indicates that the object is read only.
    -- 'rawText': Indicates that the object is a raw string.
    -- 'hasObjectId': Indicates that the object can have an Object ID created for
    -- it.
    -- 'canHaveObjectId': Indicates that the object has an Object ID associated
    -- with it.
    -- 'hasSideEffects': Indicates that the evaluation had side effects.
    -- 'hasDataBreakpoint': Indicates that the object has its value tracked by a
    -- data breakpoint.
    -- etc.
    --
  , variablePresentationHintVisibility :: Maybe PresentationHintVisibility
    -- ^
    -- Visibility of variable. Before introducing additional values, try to use
    -- the listed values.
    -- Values: 'public', 'private', 'protected', 'internal', 'final', etc.
    --
  , variablePresentationHintLazy :: Bool
    -- ^
    -- If true, clients can present the variable with a UI that supports a
    -- specific gesture to trigger its evaluation.
    -- This mechanism can be used for properties that require executing code when
    -- retrieving their value and where the code execution can be expensive and/or
    -- produce side-effects. A typical example are properties based on a getter
    -- function.
    -- Please note that in addition to the `lazy` flag, the variable's
    -- `variablesReference` is expected to refer to a variable that will provide
    -- the value through another `variable` request.
    --
  } deriving stock (Show, Eq)
----------------------------------------------------------------------------
instance ToJSON VariablePresentationHint where
  toJSON VariablePresentationHint {..} =
    object
    [ "kind"       .= variablePresentationHintKind
    , "attributes" .= variablePresentationHintAttributes
    , "visibility" .= variablePresentationHintVisibility
    , "lazy"       .= variablePresentationHintLazy
    ]
----------------------------------------------------------------------------
data PresentationHintVisibility
  = PresentationHintVisibilityPublic
  | PresentationHintVisibilityPrivate
  | PresentationHintVisibilityProtected
  | PresentationHintVisibilityInternal
  | PresentationHintVisibilityFinal
  deriving stock (Show, Eq)
----------------------------------------------------------------------------
-- | ?: 'public' | 'private' | 'protected' | 'internal' | 'final' | string;
instance ToJSON PresentationHintVisibility where
  toJSON = enumToLowerCamel (Proxy @PresentationHintVisibility)
----------------------------------------------------------------------------
data PresentationHintAttributes
  = PresentationHintAttributesStatic
  | PresentationHintAttributesConstant
  | PresentationHintAttributesReadOnly
  | PresentationHintAttributesRawText
  | PresentationHintAttributesHasObjectId
  | PresentationHintAttributesCanHaveObjectId
  | PresentationHintAttributesHasSideEffects
  | PresentationHintAttributesHasDataBreakpoint
  deriving stock (Show, Eq)
----------------------------------------------------------------------------
-- | attributes?: ('static' | 'constant' | 'readOnly' | 'rawText' | 'hasObjectId'
--   'canHaveObjectId' | 'hasSideEffects' | 'hasDataBreakpoint' | string)[];
instance ToJSON PresentationHintAttributes where
  toJSON = enumToLowerCamel (Proxy @PresentationHintAttributes)
----------------------------------------------------------------------------
data PresentationHintKind
  = PresentationHintKindProperty
  | PresentationHintKindMethod
  | PresentationHintKindClass
  | PresentationHintKindData
  | PresentationHintKindEvent
  | PresentationHintKindBaseClass
  | PresentationHintKindInnerClass
  | PresentationHintKindInterface
  | PresentationHintKindMostDerivedClass
  | PresentationHintKindVirtual
  | PresentationHintKindDataBreakpoint
  | PresentationHintKindCustom Text
  deriving stock (Show, Eq)
----------------------------------------------------------------------------
instance ToJSON PresentationHintKind where
  toJSON (PresentationHintKindCustom x) = toJSON x
  toJSON kind = enumToLowerCamel (Proxy @PresentationHintKind) kind
----------------------------------------------------------------------------
data SetVariableResponse
  = SetVariableResponse
  { setVariableResponseValue :: Text
    -- ^
    -- The new value of the variable.
    --
  , setVariableResponseType :: Maybe Text
    -- ^
    -- The type of the new value. Typically shown in the UI when hovering over
    -- the value.
    --
  , setVariableResponseReference :: Maybe Int
    -- ^
    -- If `variablesReference` is > 0, the new value is structured and its
    -- children can be retrieved by passing `variablesReference` to the
    -- `variables` request as long as execution remains suspended. See 'Lifetime
    -- of Object References' in the Overview section for details.
    --
  , setVariableResponseNamedVariables :: Maybe Int
    -- ^
    -- The number of named child variables.
    -- The client can use this information to present the variables in a paged
    -- UI and fetch them in chunks.
    -- The value should be less than or equal to 2147483647 (2^31-1).
    --

  , setVariableResponseIndexedVariables :: Maybe Int
    -- ^
    -- The number of indexed child variables.
    -- The client can use this information to present the variables in a paged
    -- UI and fetch them in chunks.
    -- The value should be less than or equal to 2147483647 (2^31-1).
    --
   } deriving stock (Show, Eq)
----------------------------------------------------------------------------
instance ToJSON SetVariableResponse where
  toJSON SetVariableResponse {..}
    = object
    [ "value"            .= setVariableResponseValue
    , "type"             .= setVariableResponseType
    , "reference"        .= setVariableResponseReference
    , "variables"        .= setVariableResponseNamedVariables
    , "indexedVariables" .= setVariableResponseIndexedVariables
    ]
----------------------------------------------------------------------------
data SourceResponse
  = SourceResponse
  { sourceResponseContent :: Text
    -- ^
    -- Content of the source reference.
    --
  , sourceResponseMimeType :: Maybe Text
    -- ^
    -- Content type (MIME type) of the source.
    --
  } deriving stock (Show, Eq)
----------------------------------------------------------------------------
instance ToJSON SourceResponse where
  toJSON SourceResponse {..}
    = object
    [ "content"  .= sourceResponseContent
    , "mimeType" .= sourceResponseMimeType
    ]
----------------------------------------------------------------------------
newtype ThreadsResponse
  = ThreadsResponse
  { threads :: [Thread]
    -- ^
    -- All threads.
    --
  } deriving stock (Show, Eq)
----------------------------------------------------------------------------
instance ToJSON ThreadsResponse where
  toJSON (ThreadsResponse ts)
    = object
    [ "threads" .= ts
    ]
----------------------------------------------------------------------------
data LoadedSourcesResponse
  = LoadedSourcesResponse
  { loadedSourcesResponseSources :: [Source]
    -- ^
    -- Set of loaded sources.
    --
  } deriving stock (Show, Eq)
----------------------------------------------------------------------------
instance ToJSON LoadedSourcesResponse where
  toJSON LoadedSourcesResponse {..}
    = object
    [ "sources" .= loadedSourcesResponseSources
    ]
----------------------------------------------------------------------------
data EvaluateResponse
  = EvaluateResponse
  { evaluateResponseResult :: Text
    -- ^
    -- The result of the evaluate request.
    --
  , evaluateResponseType :: Text
    -- ^
    -- The type of the evaluate result.
    -- This attribute should only be returned by a debug adapter if the
    -- corresponding capability `supportsVariableType` is true.
    --
  , evaluateResponsePresentationHint :: Maybe VariablePresentationHint
    -- ^
    -- Properties of an evaluate result that can be used to determine how to
    -- render the result in the UI.
    --
  , evaluateResponseVariablesReference :: Int
    -- ^
    -- If `variablesReference` is > 0, the evaluate result is structured and its
    -- children can be retrieved by passing `variablesReference` to the
    -- `variables` request as long as execution remains suspended. See 'Lifetime
    -- of Object References' in the Overview section for details.
    --
  , evaluateResponseNamedVariables :: Maybe Int
    -- ^
    -- The number of named child variables.
    -- The client can use this information to present the variables in a paged
    -- UI and fetch them in chunks.
    -- The value should be less than or equal to 2147483647 (2^31-1).
    --
  , evaluateResponseIndexedVariables :: Maybe Int
    -- ^
    -- The number of indexed child variables.
    -- The client can use this information to present the variables in a paged
    -- UI and fetch them in chunks.
    -- The value should be less than or equal to 2147483647 (2^31-1).
    --
  , evaluateResponseMemoryReference :: Maybe Text
    -- ^
    -- A memory reference to a location appropriate for this result.
    -- For pointer type eval results, this is generally a reference to the
    -- memory address contained in the pointer.
    -- This attribute should be returned by a debug adapter if corresponding
    -- capability `supportsMemoryReferences` is true.
    --
  } deriving stock (Show, Eq, Generic)
----------------------------------------------------------------------------
instance ToJSON EvaluateResponse where
  toJSON = genericToJSONWithModifier
----------------------------------------------------------------------------
data SetExpressionResponse
  = SetExpressionResponse
  { setExpressionResponseValue :: Text
    -- ^
    -- The new value of the expression.
    --
  , setExpressionResponseType :: Maybe Text
    -- ^
    -- The type of the value.
    -- This attribute should only be returned by a debug adapter if the
    -- corresponding capability `supportsVariableType` is true.
    --
  , setExpressionResponsePresentationHint :: Maybe VariablePresentationHint
    -- ^
    -- Properties of a value that can be used to determine how to render the
    -- result in the UI.
    --
  , setExpressionResponseVariablesReference:: Maybe Int
    -- ^
    -- If `variablesReference` is > 0, the evaluate result is structured and its
    -- children can be retrieved by passing `variablesReference` to the
    -- `variables` request as long as execution remains suspended. See 'Lifetime
    -- of Object References' in the Overview section for details.
    --
  , setExpressionResponseNamedVariables:: Maybe Int
    -- ^
    -- The number of named child variables.
    -- The client can use this information to present the variables in a paged
    -- UI and fetch them in chunks.
    -- The value should be less than or equal to 2147483647 (2^31-1).
    --
  , setExpressionResponseIndexedVariables:: Maybe Int
    -- ^
    -- The number of indexed child variables.
    -- The client can use this information to present the variables in a paged
    -- UI and fetch them in chunks.
    -- The value should be less than or equal to 2147483647 (2^31-1).
    --
  } deriving stock (Show, Eq, Generic)
----------------------------------------------------------------------------
instance ToJSON SetExpressionResponse where
  toJSON = genericToJSONWithModifier
----------------------------------------------------------------------------
data StepInTargetsResponse
  = StepInTargetsResponse
  { stepInTargetsResponseTargets :: [StepInTarget]
    -- ^
    -- The possible step-in targets of the specified source location.
    --
  } deriving stock (Show, Eq, Generic)
----------------------------------------------------------------------------
instance ToJSON StepInTargetsResponse where
  toJSON = genericToJSONWithModifier
----------------------------------------------------------------------------
data StepInTarget
  = StepInTarget
  { stepInTargetId :: Int
    -- ^
    -- Unique identifier for a step-in target.
    --
  , stepInTargetLabel :: Text
    -- ^
    -- The name of the step-in target (shown in the UI).
    --
  , stepInTargetLine :: Maybe Int
    -- ^
    -- The line of the step-in target.
    --
  , stepInTargetColumn :: Maybe Int
    -- ^
    -- Start position of the range covered by the step in target. It is measured
    -- in UTF-16 code units and the client capability `columnsStartAt1` determines
    -- whether it is 0- or 1-based.
    --
  , stepInTargetEndLine :: Maybe Int
    -- ^
    -- The end line of the range covered by the step-in target.
    --
  , stepInTargetEndColumn :: Int
    -- ^ End position of the range covered by the step in target. It is measured in
    -- UTF-16 code units and the client capability `columnsStartAt1` determines
    -- whether it is 0- or 1-based.
    --
  } deriving stock (Show, Eq, Generic)
----------------------------------------------------------------------------
instance ToJSON StepInTarget where
  toJSON = genericToJSONWithModifier
----------------------------------------------------------------------------
data GotoTargetsResponse
  = GotoTargetsResponse
  { goToTargetsResponseTargets :: [GotoTarget]
    -- ^
    -- The possible goto targets of the specified location.
    --
  } deriving stock (Show, Eq, Generic)
----------------------------------------------------------------------------
instance ToJSON GotoTargetsResponse where
  toJSON = genericToJSONWithModifier
----------------------------------------------------------------------------
data GotoTarget
  = GotoTarget
  { gotoTargetId :: Int
    -- ^
    -- Unique identifier for a goto target. This is used in the `goto` request.
  , gotoTargetLabel :: String
    -- ^
    -- The name of the goto target (shown in the UI).
    --
  , gotoTargetLine :: Int
    -- ^
    -- The line of the gotoTarget target.
    --
  , gotoTargetColumn :: Maybe Int
    -- ^
    -- The column of the gotoTarget target.
    --
  , gotoTargetEndLine :: Int
    -- ^
    -- The end line of the range covered by the gotoTarget target.
    --
  , gotoTargetEndColumn :: Maybe Int
    -- ^
    -- The end column of the range covered by the gotoTarget target.
    --
  , gotoTargetInstructionPointerReference :: Maybe String
    -- ^
    -- A memory reference for the instruction pointer value represented by this
    -- target.
    --
  } deriving stock (Show, Eq, Generic)
----------------------------------------------------------------------------
instance ToJSON GotoTarget where
  toJSON = genericToJSONWithModifier
----------------------------------------------------------------------------
data CompletionsResponse
  = CompletionsResponse
  { completionResponseTargets :: [CompletionItem]
    -- ^
    -- The possible completions for .
    --
  } deriving stock (Show, Eq, Generic)
----------------------------------------------------------------------------
instance ToJSON CompletionsResponse where
  toJSON = genericToJSONWithModifier
----------------------------------------------------------------------------
data CompletionItem
  = CompletionItem
  { completionItemLabel :: String
    -- ^
    -- The label of this completion item. By default this is also the text that is
    -- inserted when selecting this completion.
    --
  , completionItemText :: Maybe String
    -- ^
    -- If text is returned and not an empty string, then it is inserted instead of
    -- the label.
    --
  , completionItemSortText :: Maybe String
    -- ^
    -- A string that should be used when comparing this item with other items. If
    -- not returned or an empty string, the `label` is used instead.
    --
  , completionItemDetail :: Maybe String
    -- ^
    -- A human-readable string with additional information about this item, like
    -- type or symbol information.
    --
  , completionItemType :: Maybe CompletionItemType
    -- ^
    -- The item's type. Typically the client uses this information to render the
    -- item in the UI with an icon.
    --
  , completionItemTypeStart :: Maybe Int
    -- ^
    -- Start position (within the `text` attribute of the `completions` request)
    -- where the completion text is added. The position is measured in UTF-16 code
    -- units and the client capability `columnsStartAt1` determines whether it is
    -- 0- or 1-based. If the start position is omitted the text is added at the
    -- location specified by the `column` attribute of the `completions` request.
    --
  , completionItemTypeLength :: Maybe Int
    -- ^
    -- Length determines how many characters are overwritten by the completion
    -- text and it is measured in UTF-16 code units. If missing the value 0 is
    -- assumed which results in the completion text being inserted.
    --
  , completionItemTypeSelectionStart :: Maybe Int
    -- ^
    -- Determines the start of the new selection after the text has been inserted
    -- (or replaced). `selectionStart` is measured in UTF-16 code units and must
    -- be in the range 0 and length of the completion text. If omitted the
    -- selection starts at the end of the completion text.
    --
  , completionItemTypeSelectionLength :: Maybe Int
    -- ^
    -- Determines the length of the new selection after the text has been inserted
    -- (or replaced) and it is measured in UTF-16 code units. The selection can
    -- not extend beyond the bounds of the completion text. If omitted the length
    -- is assumed to be 0.
    --
  } deriving stock (Show, Eq, Generic)
----------------------------------------------------------------------------
instance ToJSON CompletionItem where
  toJSON = genericToJSONWithModifier
----------------------------------------------------------------------------
instance ToJSON CompletionItemType where
  toJSON = genericToJSONWithModifier
----------------------------------------------------------------------------
data ExceptionInfoResponse
  = ExceptionInfoResponse
  { exceptionInfoResponseId :: Text
    -- ^
    -- ID of the exception that was thrown.
    --
  , exceptionInfoDescriptionId :: Maybe Text
    -- ^
    -- Descriptive text for the exception.
    --
  , exceptionInfoBreakMode :: ExceptionBreakMode
    -- ^
    -- Mode that caused the exception notification to be raised.
    --
  , exceptionInfoReponseDetails :: Maybe ExceptionDetails
    -- ^
    -- Detailed information about the exception.
    --
  } deriving stock (Show, Eq, Generic)
----------------------------------------------------------------------------
instance ToJSON ExceptionInfoResponse where
  toJSON = genericToJSONWithModifier
----------------------------------------------------------------------------
data ExceptionBreakMode
  = Never
  | Always
  | Unhandled
  | UserUnhandled
  deriving stock (Show, Eq, Generic)
----------------------------------------------------------------------------
instance ToJSON ExceptionBreakMode where
  toJSON Never         = "never"
  toJSON Always        = "always"
  toJSON Unhandled     = "unhandled"
  toJSON UserUnhandled = "userUnhandled"
----------------------------------------------------------------------------
instance FromJSON ExceptionBreakMode where
  parseJSON = genericParseJSONWithModifier
----------------------------------------------------------------------------
data ExceptionDetails
  = ExceptionDetails
  { exceptionDetailsMessage :: String
    -- ^
    -- Message contained in the exception.
    --
  , exceptionDetailstypeName :: Maybe Text
    -- ^
    -- Short type name of the exception object.
    --
  , exceptionDetailsFullTypeName :: Maybe Text
    -- ^
    -- Fully-qualified type name of the exception object.
    --
  , exceptionDetailsEvaluateName :: Maybe Text
    -- ^
    -- An expression that can be evaluated in the current scope to obtain the
    -- exception object.
    --
  , exceptionDetailsStackTrace :: Maybe Text
    -- ^
    -- Stack trace at the time the exception was thrown.
    --
  , exceptionDetailsInnerException :: [ExceptionDetails]
    -- ^
    -- Details of the exception contained by this exception, if any.
    --
  } deriving stock (Show, Eq, Generic)
----------------------------------------------------------------------------
instance ToJSON ExceptionDetails where
  toJSON = genericToJSONWithModifier
----------------------------------------------------------------------------
data ReadMemoryResponse
  = ReadMemoryResponse
  { readMemoryResponseBody :: Int
    -- ^
    -- The address of the first byte of data returned.
    -- Treated as a hex value if prefixed with `0x`, or as a decimal value
    -- otherwise.
    --
  , readMemoryResponseAddress :: Text
    -- ^
    -- The number of unreadable bytes encountered after the last successfully
    -- read byte.
    -- This can be used to determine the number of bytes that should be skipped
    -- before a subsequent `readMemory` request succeeds.
    --
  , readMemoryResponseUnreadableBytes:: Maybe Int
    -- ^
    -- The bytes read from memory, encoded using base64. If the decoded length
    -- of `data` is less than the requested `count` in the original `readMemory`
    -- request, and `unreadableBytes` is zero or omitted, then the client should
    -- assume it's reached the end of readable memory.
    --
  , readMemoryResponseData :: Maybe Text
  } deriving stock (Show, Eq, Generic)
----------------------------------------------------------------------------
instance ToJSON ReadMemoryResponse where
  toJSON = genericToJSONWithModifier
----------------------------------------------------------------------------
data WriteMemoryResponse
  = WriteMemoryResponse
  { writeMemoryResponseOffset :: Maybe Int
    -- ^
    -- Property that should be returned when `allowPartial` is true to indicate
    -- the offset of the first byte of data successfully written. Can be
    -- negative.
    --
  , writeMemoryResponseBytesWritten :: Maybe Int
    -- ^
    -- Property that should be returned when `allowPartial` is true to indicate
    -- the number of bytes starting from address that were successfully written.
    --
  } deriving stock (Show, Eq)
----------------------------------------------------------------------------
instance ToJSON WriteMemoryResponse where
  toJSON WriteMemoryResponse {..}
    = object
    [ "offset"       .= writeMemoryResponseOffset
    , "bytesWritten" .= writeMemoryResponseOffset
    ]
----------------------------------------------------------------------------
data DisassembleResponse
  = DisassembleResponse
  { disassembleResponseInstructions :: [DisassembledInstruction]
    -- ^
    -- The list of disassembled instructions.
    --
  } deriving stock (Show, Eq)
----------------------------------------------------------------------------
instance ToJSON DisassembleResponse where
  toJSON (DisassembleResponse instructions)
    = object
    [ "instructions" .= instructions
    ]
----------------------------------------------------------------------------
data DisassembledInstruction
  = DisassembledInstruction
  { disassembledInstructionAddress :: Text
    -- ^
    -- The address of the instruction. Treated as a hex value if prefixed with
    -- `0x`, or as a decimal value otherwise.
  , disassembledInstructionInstructionBytes :: Text
    -- ^
    -- Raw bytes representing the instruction and its operands, in an
    -- implementation-defined format.
  , disassembledInstructionInstruction :: Text
    -- ^
    -- Text representing the instruction and its operands, in an
    -- implementation-defined format.
    --
  , disassembledInstructionSymbol :: Text
    -- ^
    -- Name of the symbol that corresponds with the location of this instruction,
    -- if any.
    --
  , disassembledInstructionLocation :: Maybe Source
    -- ^
    -- Source location that corresponds to this instruction, if any.
    -- Should always be set (if available) on the first instruction returned,
    -- but can be omitted afterwards if this instruction maps to the same source
    -- file as the previous instruction.
    --
  , disassembledInstructionLine :: Maybe Int
    -- ^
    -- The line within the source location that corresponds to this instruction,
    -- if any.
    --
  , disassembledInstructionColumn :: Maybe Int
    -- ^
    -- The column within the line that corresponds to this instruction, if any.
    --
  , disassembledInstructionEndLine :: Maybe Int
    -- ^
    -- The end line of the range that corresponds to this instruction, if any.
    --
  , disassembledInstructionEndColumn :: Maybe Int
    -- ^
    -- The end column of the range that corresponds to this instruction, if any.
    --
  } deriving stock (Show, Eq, Generic)
----------------------------------------------------------------------------
instance ToJSON DisassembledInstruction where
  toJSON = genericToJSONWithModifier
----------------------------------------------------------------------------
data StoppedEventReason
  = StoppedEventReasonStep
  | StoppedEventReasonBreakpoint
  | StoppedEventReasonException
  | StoppedEventReasonPause
  | StoppedEventReasonEntry
  | StoppedEventReasonGoto
  | StoppedEventReasonFunctionBreakpoint
  | StoppedEventReasonDataBreakpoint
  | StoppedEventReasonInstructionBreakpoint
  deriving stock (Show, Eq)
----------------------------------------------------------------------------
instance ToJSON StoppedEventReason where
  toJSON StoppedEventReasonStep                  = "step"
  toJSON StoppedEventReasonBreakpoint            = "breakpoint"
  toJSON StoppedEventReasonException             = "exception"
  toJSON StoppedEventReasonPause                 = "pause"
  toJSON StoppedEventReasonEntry                 = "entry"
  toJSON StoppedEventReasonGoto                  = "goto"
  toJSON StoppedEventReasonFunctionBreakpoint    = "function breakpoint"
  toJSON StoppedEventReasonDataBreakpoint        = "data breakpoint"
  toJSON StoppedEventReasonInstructionBreakpoint = "instruction breakpoint"
----------------------------------------------------------------------------
data StoppedEvent
  = StoppedEvent
  { stoppedEventReason :: StoppedEventReason
    -- ^
    -- The reason for the event.
    -- For backward compatibility this string is shown in the UI if the
    -- `description` attribute is missing (but it must not be translated).
    -- Values: 'step', 'breakpoint', 'exception', 'pause', 'entry', 'goto',
    -- 'function breakpoint', 'data breakpoint', 'instruction breakpoint', etc.
    --
  , stoppedEventDescription :: Maybe Text
    -- ^
    -- The full reason for the event, e.g. 'Paused on exception'. This string is
    -- shown in the UI as is and can be translated.
    --
  , stoppedEventThreadId :: Maybe Int
    -- ^
    -- The thread which was stopped.
    --
  , stoppedEventPreserveFocusHint :: Bool
    -- ^
    -- A value of true hints to the client that this event should not change the
    -- focus.
    --
  , stoppedEventText :: Maybe Text
    -- ^
    -- Additional information. E.g. if reason is `exception`, text contains the
    -- exception name. This string is shown in the UI.
    --
  , stoppedEventAllThreadsStopped :: Bool
    -- ^
    -- If `allThreadsStopped` is true, a debug adapter can announce that all
    -- threads have stopped.
    -- - The client should use this information to enable that all threads can
    -- be expanded to access their stacktraces.
    -- - If the attribute is missing or false, only the thread with the given
    -- `threadId` can be expanded.
    --
  , stoppedEventHitBreakpointIds :: [Int]
    -- ^
    -- Ids of the breakpoints that triggered the event. In most cases there is
    -- only a single breakpoint but here are some examples for multiple
    -- breakpoints:
    -- - Different types of breakpoints map to the same location.
    -- - Multiple source breakpoints get collapsed to the same instruction by
    -- the compiler/runtime.
    -- - Multiple function breakpoints with different function names map to the
    -- same location.
    --
  } deriving stock (Show, Eq)
----------------------------------------------------------------------------
instance ToJSON StoppedEvent where
  toJSON StoppedEvent{..}
    = object
      [ "reason"            .= stoppedEventReason
      , "description"       .= stoppedEventDescription
      , "threadId"          .= stoppedEventThreadId
      , "preserveFocusHint" .= stoppedEventPreserveFocusHint
      , "text"              .= stoppedEventText
      , "allThreadsStopped" .= stoppedEventAllThreadsStopped
      , "breakpointIds"     .= stoppedEventHitBreakpointIds
      ]
----------------------------------------------------------------------------
data ContinuedEvent
  = ContinuedEvent
  { continuedEventThreadId :: Int
    -- ^
    -- The thread which was continued.
    --
  , continuedEventAllThreadsContinued :: Bool
    -- ^
    -- If `allThreadsContinued` is true, a debug adapter can announce that all
    -- threads have continued.
    --
  } deriving stock (Show, Eq)
----------------------------------------------------------------------------
instance ToJSON ContinuedEvent where
  toJSON ContinuedEvent {..}
    = object
    [ "threadId"            .= continuedEventThreadId
    , "allThreadsContinued" .= continuedEventAllThreadsContinued
    ]
----------------------------------------------------------------------------
data ExitedEvent
  = ExitedEvent
  { exitedEventExitCode :: Int
    -- ^
    -- The exit code returned from the debuggee.
    --
  } deriving stock (Show, Eq)
----------------------------------------------------------------------------
instance ToJSON ExitedEvent where
  toJSON ExitedEvent {..}
    = object
    [ "exitCode" .= exitedEventExitCode
    ]
----------------------------------------------------------------------------
data TerminatedEvent
  = TerminatedEvent
  { terminatedEventRestart :: Bool
    -- ^
    -- A debug adapter may set `restart` to true (or to an arbitrary object) to
    -- request that the client restarts the session.
    -- The value is not interpreted by the client and passed unmodified as an
    -- attribute `__restart` to the `launch` and `attach` requests.
    --
  } deriving stock (Show, Eq)
----------------------------------------------------------------------------
instance ToJSON TerminatedEvent where
  toJSON TerminatedEvent {..}
    = object
    [ "restart" .= terminatedEventRestart
    ]
----------------------------------------------------------------------------
data ThreadEvent
  = ThreadEvent
  { threadEventReason :: ThreadEventReason
    -- ^
    -- The reason for the event.
    -- Values: 'started', 'exited', etc.
    --
  , threadEventId :: Int
    -- ^
    -- The identifier of the thread.
    --
  } deriving stock (Show, Eq)
----------------------------------------------------------------------------
instance ToJSON ThreadEvent where
  toJSON ThreadEvent {..}
    = object
    [ "reason"   .= threadEventReason
    , "threadId" .= threadEventId
    ]
----------------------------------------------------------------------------
data ThreadEventReason
  = Started
  | Exited
  | CustomThreadEventReason Text
  deriving (Show, Eq)
----------------------------------------------------------------------------
instance ToJSON ThreadEventReason where
  toJSON Started                          = toJSON ("started" :: Text)
  toJSON Exited                           = toJSON ("exited" :: Text)
  toJSON (CustomThreadEventReason custom) = toJSON custom
----------------------------------------------------------------------------
data OutputEventCategory
  = Console
  | Important
  | Stdout
  | Stderr
  | Telemetry
  | CustomOutputEvent Text
  deriving stock (Show, Eq)
----------------------------------------------------------------------------
data EventGroup
  = Start
  | StartCollapsed
  | End
  deriving stock (Show, Eq)
----------------------------------------------------------------------------
instance ToJSON EventGroup where
  toJSON = toJSON . enumToLowerCamel (Proxy @EventGroup)
----------------------------------------------------------------------------
data OutputEvent
  = OutputEvent
  { outputEventCategory :: Maybe OutputEvent
    -- ^
    -- The output category. If not specified or if the category is not
    -- understood by the client, `console` is assumed.
    -- Values:
    -- 'console': Show the output in the client's default message UI, e.g. a
    -- 'debug console'. This category should only be used for informational
    -- output from the debugger (as opposed to the debuggee).
    -- 'important': A hint for the client to show the output in the client's UI
    -- for important and highly visible information, e.g. as a popup
    -- notification. This category should only be used for important messages
    -- from the debugger (as opposed to the debuggee). Since this category value
    -- is a hint, clients might ignore the hint and assume the `console`
    -- category.
    -- 'stdout': Show the output as normal program output from the debuggee.
    -- 'stderr': Show the output as error program output from the debuggee.
    -- 'telemetry': Send the output to telemetry instead of showing it to the
    -- user.
    -- etc.
    --
  , outputEventOutput :: Text
    -- ^
    -- The output to report.
    --
  , outputEventGroup :: Maybe EventGroup
    -- ^
    -- Support for keeping an output log organized by grouping related messages.
    -- Values:
    -- 'start': Start a new group in expanded mode. Subsequent output events are
    -- members of the group and should be shown indented.
    -- The `output` attribute becomes the name of the group and is not indented.
    -- 'startCollapsed': Start a new group in collapsed mode. Subsequent output
    -- events are members of the group and should be shown indented (as soon as
    -- the group is expanded).
    -- The `output` attribute becomes the name of the group and is not indented.
    -- 'end': End the current group and decrease the indentation of subsequent
    -- output events.
    -- A non-empty `output` attribute is shown as the unindented end of the
    -- group.
    --
  , outputEventVariablesReference :: Maybe Int
    -- ^
    -- If an attribute `variablesReference` exists and its value is > 0, the
    -- output contains objects which can be retrieved by passing
    -- `variablesReference` to the `variables` request as long as execution
    -- remains suspended. See 'Lifetime of Object References' in the Overview
    -- section for details.
    --
  , outputEventSource :: Maybe Source
    -- ^
    -- The source location where the output was produced.
    --
  , outputEventLine :: Maybe Int
    -- ^
    -- The source location's line where the output was produced.
    --
  , outputEventColumn :: Maybe Int
    -- ^
    -- The position in `line` where the output was produced. It is measured in
    -- UTF-16 code units and the client capability `columnsStartAt1` determines
    -- whether it is 0- or 1-based.
    --
  , outputEventData :: Maybe Value
    -- ^
    -- Additional data to report. For the `telemetry` category the data is sent
    -- to telemetry, for the other categories the data is shown in JSON format.
    --
  } deriving stock (Show, Eq)
----------------------------------------------------------------------------
instance ToJSON OutputEvent where
  toJSON OutputEvent{..} =
    object
      [ "category"           .= outputEventCategory
      , "output"             .= outputEventOutput
      , "group"              .= outputEventGroup
      , "variablesReference" .= outputEventVariablesReference
      , "source"             .= outputEventSource
      , "line"               .= outputEventLine
      , "column"             .= outputEventColumn
      , "data"               .= outputEventData
      ]
----------------------------------------------------------------------------
data BreakpointEvent
  = BreakpointEvent
  { breakpointEventReason :: EventReason
    -- ^
    -- The reason for the event.
    -- Values: 'changed', 'new', 'removed', etc.
    --
  , breakpointEvevntBreakpoint :: Breakpoint
    -- ^
    -- The `id` attribute is used to find the target breakpoint, the other
    -- attributes are used as the new values.
    --
  } deriving stock (Show, Eq)
----------------------------------------------------------------------------
data EventReason
  = New
  | Changed
  | Removed
  | EventReason Text
  deriving stock (Show, Eq)
----------------------------------------------------------------------------
instance ToJSON EventReason where
  toJSON New                  = "new"
  toJSON Changed              = "changed"
  toJSON Removed              = "removed"
  toJSON (EventReason reason) = toJSON reason
----------------------------------------------------------------------------
data ModuleEvent
  = ModuleEvent
  { moduleEventReason :: EventReason
    -- ^
    -- The reason for the event.
    -- Values: 'new', 'changed', 'removed'
    --
  , moduleEventModule :: Module
    -- ^
    -- The new, changed, or removed module. In case of `removed` only the module
    -- id is used.
    --
  } deriving stock (Show, Eq)
----------------------------------------------------------------------------
instance ToJSON ModuleEvent where
  toJSON ModuleEvent{..} =
    object
      [ "reason" .= moduleEventReason
      , "module" .= moduleEventModule
      ]
----------------------------------------------------------------------------
data LoadedSourceEvent
  = LoadedSourceEvent
  { loadedSourceEventReason :: EventReason
    -- ^
    -- The reason for the event.
    -- Values: 'new', 'changed', 'removed'
    --
  , loadedSourceSource :: Source
    -- ^
    -- The new, changed, or removed source.
    --
  } deriving stock (Show, Eq)
----------------------------------------------------------------------------
instance ToJSON LoadedSourceEvent where
  toJSON LoadedSourceEvent{..}
    = object
    [ "reason" .= loadedSourceEventReason
    , "source" .= loadedSourceSource
    ]
----------------------------------------------------------------------------
data ProcessEvent
  = ProcessEvent
  { processEventName :: Text
    -- ^
    -- The logical name of the process. This is usually the full path to
    -- process's executable file. Example: /home/example/myproj/program.js.
    --
  , processEventSystemProcessId :: Maybe Int
    -- ^
    -- The system process id of the debugged process. This property is missing
    -- for non-system processes.
    --
  , processEventIsLocalProcess :: Bool
    -- ^
    -- If true, the process is running on the same computer as the debug
    -- adapter.
    --
  , processEventStartMethod :: Maybe StartMethod
    -- ^
    -- Describes how the debug engine started debugging this process.
    -- Values:
    -- 'launch': Process was launched under the debugger.
    -- 'attach': Debugger attached to an existing process.
    -- 'attachForSuspendedLaunch': A project launcher component has launched a
    -- new process in a suspended state and then asked the debugger to attach.
    --
  , processEventPointerSize :: Maybe Int
    -- ^
    -- The size of a pointer or address for this process, in bits. This value
    -- may be used by clients when formatting addresses for display.
    --
  } deriving stock (Show, Eq)
----------------------------------------------------------------------------
instance ToJSON ProcessEvent where
  toJSON ProcessEvent{..} =
    object
      [ "name"           .= processEventName
      , "processId"      .= processEventSystemProcessId
      , "isLocalProcess" .= processEventIsLocalProcess
      , "startMethod"    .= processEventStartMethod
      , "pointerSize"    .= processEventPointerSize
      ]
----------------------------------------------------------------------------
data StartMethod
  = Launch
  | Attach
  | AttachForSuspendedLaunch
   deriving stock (Show, Eq)
----------------------------------------------------------------------------
instance ToJSON StartMethod where
  toJSON = toJSON . toLowerCase . show
----------------------------------------------------------------------------
data CapabilitiesEvent
  = CapabilitiesEvent
  { capabilities :: Capabilities
    -- ^
    -- The set of updated capabilities.
    --
  } deriving stock (Show, Eq)
----------------------------------------------------------------------------
data ProgressStartEvent
  = ProgressStartEvent
  { progressStartEventProgressId :: Text
    -- ^
    -- An ID that can be used in subsequent `progressUpdate` and `progressEnd`
    -- events to make them refer to the same progress reporting.
    -- IDs must be unique within a debug session.
    --
  , progressStartEventTitle :: Text
    -- ^
    -- Short title of the progress reporting. Shown in the UI to describe the
    -- long running operation.
    --
  , progressStartEventRequestId :: Maybe Int
    -- ^
    -- The request ID that this progress report is related to. If specified a
    -- debug adapter is expected to emit progress events for the long running
    -- request until the request has been either completed or cancelled.
    -- If the request ID is omitted, the progress report is assumed to be
    -- related to some general activity of the debug adapter.
    --
  , progressStartEventCancellable :: Bool
    -- ^
    -- If true, the request that reports progress may be cancelled with a
    -- `cancel` request.
    -- So this property basically controls whether the client should use UX that
    -- supports cancellation.
    -- Clients that don't support cancellation are allowed to ignore the
    -- setting.
    --
  , progressStartEventMessage :: Maybe Text
    -- ^
    -- More detailed progress message.
    --
  , progressStartEventPercentage :: Maybe Int
    -- ^
    -- Progress percentage to display (value range: 0 to 100). If omitted no
    -- percentage is shown.
    --
  } deriving stock (Show, Eq)
----------------------------------------------------------------------------
instance ToJSON ProgressStartEvent where
  toJSON ProgressStartEvent {..}
    = object
    [ "progressId"  .= progressStartEventProgressId
    , "eventTitle"  .= progressStartEventTitle
    , "requestId"   .= progressStartEventRequestId
    , "cancellable" .= progressStartEventCancellable
    , "message"     .= progressStartEventMessage
    , "percentage"  .= progressStartEventPercentage
    ]
----------------------------------------------------------------------------
data ProgressUpdateEvent
  = ProgressUpdateEvent
  { progressUpdateEventProgressId :: Text
    -- ^
    -- The ID that was introduced in the initial `progressStart` event.
    --
  , progressUpdateEventMessage :: Maybe Text
    -- ^
    -- More detailed progress message. If omitted, the previous message (if any)
    -- is used.
    --
  , progressUpdateEventPercentage :: Maybe Int
    -- ^
    -- Progress percentage to display (value range: 0 to 100). If omitted no
    -- percentage is shown.
    --
  } deriving stock (Show, Eq)
----------------------------------------------------------------------------
instance ToJSON ProgressUpdateEvent where
  toJSON ProgressUpdateEvent {..}
    = object
    [ "progressId"  .= progressUpdateEventProgressId
    , "message"     .= progressUpdateEventMessage
    , "percentage"  .= progressUpdateEventPercentage
    ]
----------------------------------------------------------------------------
data ProgressEndEvent
  = ProgressEndEvent
  { progressEndEventProgressId :: Text
    -- ^
    -- The ID that was introduced in the initial `ProgressStartEvent`.
    --
  , progressEndEventMessage :: Maybe Text
    -- ^
    -- More detailed progress message. If omitted, the previous message (if any)
    -- is used.
    --
  } deriving stock (Show, Eq)
----------------------------------------------------------------------------
instance ToJSON ProgressEndEvent where
  toJSON ProgressEndEvent{..}
    = object
    [ "progressId" .= progressEndEventProgressId
    , "message"    .= progressEndEventMessage
    ]
----------------------------------------------------------------------------
data InvalidatedEvent
  = InvalidatedEvent
  { invalidatedEventAreas :: [InvalidatedAreas]
     -- ^
     -- Set of logical areas that got invalidated. This property has a hint
     -- characteristic: a client can only be expected to make a 'best effort' in
     -- honoring the areas but there are no guarantees. If this property is
     -- missing, empty, or if values are not understood, the client should assume
     -- a single value `all`.
     --
  , invalidatedEventThreadId :: Maybe Int
     -- ^
     -- If specified, the client only needs to refetch data related to this
     -- thread.
     --
  , invalidatedEventStackFrameId :: Maybe Int
     -- ^
     -- If specified, the client only needs to refetch data related to this stack
     -- frame (and the `threadId` is ignored).
     --
  } deriving stock (Show, Eq)
----------------------------------------------------------------------------
instance ToJSON InvalidatedEvent where
  toJSON InvalidatedEvent{..} =
    object
      [ "areas"        .= invalidatedEventAreas
      , "threadId"     .= invalidatedEventThreadId
      , "stackFrameId" .= invalidatedEventStackFrameId
      ]
----------------------------------------------------------------------------
-- | Logical areas that can be invalidated by the invalidated event. Values:
-- <https://microsoft.github.io/debug-adapter-protocol/specification#Types_InvalidatedAreas>
--
data InvalidatedAreas
  = InvalidatedAreasAll
  | InvalidatedAreasStacks
  | InvalidatedAreasThreads
  | InvalidatedAreasVariables
  deriving stock (Show, Eq)
----------------------------------------------------------------------------
instance ToJSON InvalidatedAreas where
  toJSON = enumToLowerCamel (Proxy @InvalidatedAreas)
----------------------------------------------------------------------------
data MemoryEvent
  = MemoryEvent
  { memoryEventMemoryReference :: Text
    -- ^
    -- Memory reference of a memory range that has been updated.
    --
  , memoryEventOffset :: Int
    -- ^
    -- Starting offset in bytes where memory has been updated. Can be negative.
    --
  , memoryEventCount :: Int
    -- ^
    -- Number of bytes updated.
    --
  } deriving stock (Show, Eq)
----------------------------------------------------------------------------
instance ToJSON MemoryEvent where
  toJSON MemoryEvent{..} =
    object
      [ "memoryReference" .= memoryEventMemoryReference
      , "offset"          .= memoryEventOffset
      , "count"           .= memoryEventCount
      ]
----------------------------------------------------------------------------
data CancelArguments
  = CancelArguments
  { cancelArgumentsRequestId :: Maybe Int
    -- ^
    -- The ID (attribute `seq`) of the request to cancel. If missing no request is
    -- cancelled.
    -- Both a `requestId` and a `progressId` can be specified in one request.
    --
  , cancelArgumentsProgressId :: Maybe Text
    -- ^
    -- The ID (attribute `progressId`) of the progress to cancel. If missing no
    -- progress is cancelled.
    -- Both a `requestId` and a `progressId` can be specified in one request.
    --
  } deriving stock (Show, Eq, Generic)
----------------------------------------------------------------------------
instance FromJSON CancelArguments where
  parseJSON = genericParseJSONWithModifier
----------------------------------------------------------------------------
data RunInTerminalRequestArgumentsKind
  = RunInTerminalRequestArgumentsKindIntegrated
  | RunInTerminalRequestArgumentsKindExternal
  deriving stock (Show, Eq, Generic)
----------------------------------------------------------------------------
instance FromJSON RunInTerminalRequestArgumentsKind where
  parseJSON = genericParseJSONWithModifier
----------------------------------------------------------------------------
data RunInTerminalRequestArguments
  = RunInTerminalRequestArguments
  { runInTerminalRequestArgumentsKind :: Maybe RunInTerminalRequestArgumentsKind
    -- ^
    -- What kind of terminal to launch. Defaults to `integrated` if not specified.
    -- Values: 'integrated', 'external'
    --
  , runInTerminalRequestArgumentsTitle :: Maybe Text
    -- ^
    -- Title of the terminal.
    --
  , runInTerminalRequestArgumentsCwd :: Text
    -- ^
    -- Working directory for the command. For non-empty, valid paths this
    -- typically results in execution of a change directory command.
    --
  , runInTerminalRequestArgumentsArgs :: [Text]
    -- ^
    -- List of arguments. The first argument is the command to run.
    --
    -- .:? "foo" .!= mempty
  , runInTerminalRequestArgumentsEnv :: Maybe (H.HashMap Text Text)
    -- ^
    -- Environment key-value pairs that are added to or removed from the default
    -- environment.
    --
  , runInTerminalRequestArgumentsArgsCanBeInterpretedByShell :: Bool
    -- ^
    -- This property should only be set if the corresponding capability
    -- `supportsArgsCanBeInterpretedByShell` is true. If the client uses an
    -- intermediary shell to launch the application, then the client must not
    -- attempt to escape characters with special meanings for the shell. The user
    -- is fully responsible for escaping as needed and that arguments using
    -- special characters may not be portable across shells.
    --
  } deriving stock (Show, Eq, Generic)
----------------------------------------------------------------------------
instance FromJSON RunInTerminalRequestArguments where
  parseJSON = genericParseJSONWithModifier
----------------------------------------------------------------------------
data StartDebuggingRequestArgumentsConfiguration
  = StartDebuggingRequestArgumentsConfigurationLaunch
  | StartDebuggingRequestArgumentsConfigurationAttach
  deriving stock (Show, Eq, Generic)
----------------------------------------------------------------------------
instance FromJSON StartDebuggingRequestArgumentsConfiguration where
  parseJSON = genericParseJSONWithModifier
----------------------------------------------------------------------------
data StartDebuggingRequestArguments
  = StartDebuggingRequestArguments
  { startDebuggingRequestArgumentsConfiguration :: H.HashMap Text Value
    -- ^
    -- Arguments passed to the new debug session. The arguments must only contain
    -- properties understood by the `launch` or `attach` requests of the debug
    -- adapter and they must not contain any client-specific properties (e.g.
    -- `type`) or client-specific features (e.g. substitutable 'variables').
    --
  , startDebuggingRequestArgumentsConfigurationRequest :: StartDebuggingRequestArgumentsConfiguration
    -- ^
    -- Indicates whether the new debug session should be started with a `launch`
    -- or `attach` request.
    -- Values: 'launch', 'attach'
    --
  } deriving stock (Show, Eq, Generic)
----------------------------------------------------------------------------
instance FromJSON StartDebuggingRequestArguments where
  parseJSON = genericParseJSONWithModifier
----------------------------------------------------------------------------
data PathFormat
  = Path
  | URI
  | PathFormat Text
  deriving stock (Show, Eq)
----------------------------------------------------------------------------
instance FromJSON PathFormat where
   parseJSON = withText "PathFormat" $ \txt ->
     pure $ case txt of
       "path" -> Path
       "uri"  -> URI
       _      -> PathFormat txt
----------------------------------------------------------------------------
data InitializeRequestArguments
  = InitializeRequestArguments
  { clientID :: Maybe Text
    -- ^
    -- The ID of the client using this adapter.
    --
  , clientName :: Maybe Text
    -- ^
    -- The human-readable name of the client using this adapter.
    --
  , adapterID :: Text
    -- ^
    -- The ID of the debug adapter.
    --
  , locale :: Maybe Text
    -- ^
    -- The ISO-639 locale of the client using this adapter, e.g. en-US or de-CH.
    --
  , linesStartAt1 :: Bool
    -- ^
    -- If true all line numbers are 1-based (default).
    --
  , columnsStartAt1 :: Bool
    -- ^
    -- If true all column numbers are 1-based (default).
    --
  , pathFormat :: Maybe PathFormat
    -- ^
    -- Determines in what format paths are specified. The default is `path`, which
    -- is the native format.
    -- Values: 'path', 'uri', etc.
    --
  , supportsVariableType :: Bool
    -- ^
    -- Client supports the `type` attribute for variables.
    --
  , supportsVariablePaging :: Bool
    -- ^
    -- Client supports the paging of variables.
    --
  , supportsRunInTerminalRequest :: Bool
    -- ^
    -- Client supports the `runInTerminal` request.
    --
  , supportsMemoryReferences :: Bool
    -- ^
    -- Client supports memory references.
    --
  , supportsProgressReporting :: Bool
    -- ^
    -- Client supports progress reporting.
    --
  , supportsInvalidatedEvent :: Bool
    -- ^
    -- Client supports the `invalidated` event.
    --
  , supportsMemoryEvent :: Bool
    -- ^
    -- Client supports the `memory` event.
    --
  , supportsArgsCanBeInterpretedByShell :: Bool
    -- ^
    -- Client supports the `argsCanBeInterpretedByShell` attribute on the
    -- `runInTerminal` request.
    --
  , supportsStartDebuggingRequest :: Bool
    -- ^
    -- Client supports the `startDebugging` request.
    --
  } deriving stock (Show, Eq, Generic)
----------------------------------------------------------------------------
instance FromJSON InitializeRequestArguments where
  parseJSON = genericParseJSON defaultOptions
----------------------------------------------------------------------------
data LaunchRequestArguments
  = LaunchRequestArguments
  { launchRequestArgumentsNoDebug :: Bool
    -- ^
    -- If true, the launch request should launch the program without enabling
    -- debugging.
    --
  , launchRequestArgumentsRestart :: Maybe Value
    -- ^
    -- Arbitrary data from the previous, restarted session.
    -- The data is sent as the `restart` attribute of the `terminated` event.
    -- The client should leave the data intact.
    --
  } deriving stock (Show, Eq, Generic)
----------------------------------------------------------------------------
instance FromJSON LaunchRequestArguments where
  parseJSON = genericParseJSONWithModifier
----------------------------------------------------------------------------
data AttachRequestArguments
  = AttachRequestArguments
  { attachRequestArgumentsRestart :: Maybe Value
    -- ^
    -- Arbitrary data from the previous, restarted session.
    -- The data is sent as the `restart` attribute of the `terminated` event.
    -- The client should leave the data intact.
    --
  } deriving stock (Show, Eq, Generic)
----------------------------------------------------------------------------
instance FromJSON AttachRequestArguments where
  parseJSON = genericParseJSONWithModifier
----------------------------------------------------------------------------
data RestartArguments
  = RestartArguments
  { restartArgumentsArguments :: Maybe (Either LaunchRequestArguments AttachRequestArguments)
    -- ^
    -- The latest version of the `launch` or `attach` configuration.
    --
  } deriving stock (Show, Eq, Generic)
----------------------------------------------------------------------------
instance FromJSON RestartArguments where
  parseJSON = withObject "RestartArguments" $ \o -> do
     o .:? "arguments" >>= \case
       Nothing ->
         pure (RestartArguments Nothing)
       Just r -> do
         value <- Left <$> parseJSON r <|> Right <$> parseJSON r
         pure $ RestartArguments (Just value)
----------------------------------------------------------------------------
data DisconnectArguments
  = DisconnectArguments
  { disconnectArgumentsRestart :: Bool
    -- ^
    -- A value of true indicates that this `disconnect` request is part of a
    -- restart sequence.
    --
  , disconnectArgumentsTerminateDebuggee :: Bool
    -- ^
    -- Indicates whether the debuggee should be terminated when the debugger is
    -- disconnected.
    -- If unspecified, the debug adapter is free to do whatever it thinks is best.
    -- The attribute is only honored by a debug adapter if the corresponding
    -- capability `supportTerminateDebuggee` is true.
    --
  , disconnectArgumentsSuspendDebuggee :: Bool
    -- ^
    -- Indicates whether the debuggee should stay suspended when the debugger is
    -- disconnected.
    -- If unspecified, the debuggee should resume execution.
    -- The attribute is only honored by a debug adapter if the corresponding
    -- capability `supportSuspendDebuggee` is true.
    --
  } deriving stock (Show, Eq, Generic)
----------------------------------------------------------------------------
instance FromJSON DisconnectArguments where
  parseJSON = genericParseJSONWithModifier
----------------------------------------------------------------------------
data TerminateArguments
  = TerminateArguments
  { terminateArgumentsRestart :: Bool
    -- ^
    -- A value of true indicates that this `terminate` request is part of a
    -- restart sequence.
    --
  } deriving stock (Show, Eq, Generic)
----------------------------------------------------------------------------
instance FromJSON TerminateArguments where
  parseJSON = genericParseJSONWithModifier
----------------------------------------------------------------------------
data BreakpointLocationsArguments
  = BreakpointLocationsArguments
  { breakpointLocationsArgumentsSource :: Source
    -- ^
    -- The source location of the breakpoints; either `source.path` or
    -- `source.reference` must be specified.
    --
  , breakpointLocationsArgumentsLine :: Int
    -- ^
    -- Start line of range to search possible breakpoint locations in. If only the
    -- line is specified, the request returns all possible locations in that line.
    --
  , breakpointLocationsArgumentsColumn :: Maybe Int
    -- ^
    -- Start position within `line` to search possible breakpoint locations in. It
    -- is measured in UTF-16 code units and the client capability
    -- `columnsStartAt1` determines whether it is 0- or 1-based. If no column is
    -- given, the first position in the start line is assumed.
    --
  , breakpointLocationsArgumentsEndLine :: Maybe Int
    -- ^
    -- End line of range to search possible breakpoint locations in. If no end
    -- line is given, then the end line is assumed to be the start line.
    --
  , breakpointLocationsArgumentsEndColumn :: Maybe Int
    -- ^
    -- End position within `endLine` to search possible breakpoint locations in.
    -- It is measured in UTF-16 code units and the client capability
    -- `columnsStartAt1` determines whether it is 0- or 1-based. If no end column
    -- is given, the last position in the end line is assumed.
    --
  } deriving stock (Show, Eq, Generic)
----------------------------------------------------------------------------
instance FromJSON BreakpointLocationsArguments where
  parseJSON = genericParseJSONWithModifier
----------------------------------------------------------------------------
data SetBreakpointsArguments
  = SetBreakpointsArguments
  { setBreakpointsArgumentsSource :: Source
    -- ^
    -- The source location of the breakpoints; either `source.path` or
    -- `source.sourceReference` must be specified.
    --
  , setBreakpointsArgumentsBreakpoints :: Maybe [SourceBreakpoint] -- use .!=
    -- ^
    -- The code locations of the breakpoints.
    --
  , setBreakpointArgumentsLines :: Maybe [Int] -- use .!=
    -- ^
    -- Deprecated: The code locations of the breakpoints.
    --
  , sourceModified :: Bool
    -- ^
    -- A value of true indicates that the underlying source has been modified
    -- which results in new breakpoint locations.
    --
  } deriving stock (Show, Eq, Generic)
----------------------------------------------------------------------------
instance FromJSON SetBreakpointsArguments where
  parseJSON = genericParseJSONWithModifier
----------------------------------------------------------------------------
data SourceBreakpoint
  = SourceBreakpoint
  { sourceBreakpointLine :: Int
    -- ^
    -- The source line of the breakpoint or logpoint.
    --
  , sourceBreakpointColumn :: Maybe Int
    -- ^
    -- Start position within source line of the breakpoint or logpoint. It is
    -- measured in UTF-16 code units and the client capability `columnsStartAt1`
    -- determines whether it is 0- or 1-based.
    --
  , sourceBreakpointCondition :: Maybe Text
    -- ^
    -- The expression for conditional breakpoints.
    -- It is only honored by a debug adapter if the corresponding capability
    -- `supportsConditionalBreakpoints` is true.
    --
  , sourceBreakpointHitCondition :: Maybe Text
    -- ^
    -- The expression that controls how many hits of the breakpoint are ignored.
    -- The debug adapter is expected to interpret the expression as needed.
    -- The attribute is only honored by a debug adapter if the corresponding
    -- capability `supportsHitConditionalBreakpoints` is true.
    -- If both this property and `condition` are specified, `hitCondition` should
    -- be evaluated only if the `condition` is met, and the debug adapter should
    -- stop only if both conditions are met.
    --
  , sourceBreakpointLogMessage :: Text
    -- ^
    -- If this attribute exists and is non-empty, the debug adapter must not
    -- 'break' (stop)
    -- but log the message instead. Expressions within `{}` are interpolated.
    -- The attribute is only honored by a debug adapter if the corresponding
    -- capability `supportsLogPoints` is true.
    -- If either `hitCondition` or `condition` is specified, then the message
    -- should only be logged if those conditions are met.
    --
  } deriving stock (Show, Eq, Generic)
----------------------------------------------------------------------------
instance FromJSON SourceBreakpoint where
  parseJSON = genericParseJSONWithModifier
----------------------------------------------------------------------------
data SetFunctionBreakpointsArguments
  = SetFunctionBreakpointsArguments
  { setFunctionBreakpointsArgumentsBreakpoints :: [FunctionBreakpoint]
    -- ^
    -- The function names of the breakpoints.
    --
  } deriving stock (Show, Eq, Generic)
----------------------------------------------------------------------------
instance FromJSON SetFunctionBreakpointsArguments where
  parseJSON = genericParseJSONWithModifier
----------------------------------------------------------------------------
data FunctionBreakpoint
  = FunctionBreakpoint
  { functionBreakpointName :: Maybe Text
    -- ^
    -- The name of the function.
    --
  , functionBreakpointCondition :: Text
    -- ^
    -- An expression for conditional breakpoints.
    -- It is only honored by a debug adapter if the corresponding capability
    -- `supportsConditionalBreakpoints` is true.
    --
  } deriving stock (Show, Eq, Generic)
----------------------------------------------------------------------------
instance FromJSON FunctionBreakpoint where
  parseJSON = genericParseJSONWithModifier
----------------------------------------------------------------------------
data SetExceptionBreakpointsArguments
  = SetExceptionBreakpointsArguments
  { setExceptionBreakpointsArgumentsFilters :: [Text]
    -- ^
    -- Set of exception filters specified by their ID. The set of all possible
    -- exception filters is defined by the `exceptionBreakpointFilters`
    -- capability. The `filter` and `filterOptions` sets are additive.
    --
  , setExceptionBreakpointsArgumentsFilterOptions :: Maybe ExceptionFilterOptions -- .!=
    -- ^
    -- Set of exception filters and their options. The set of all possible
    -- exception filters is defined by the `exceptionBreakpointFilters`
    -- capability. This attribute is only honored by a debug adapter if the
    -- corresponding capability `supportsExceptionFilterOptions` is true. The
    -- `filter` and `filterOptions` sets are additive.
    --
  , setExceptionBreakpointsArgumentsExceptionOptions :: Maybe ExceptionOptions
   -- ^
   -- Configuration options for selected exceptions.
   -- The attribute is only honored by a debug adapter if the corresponding
   -- capability `supportsExceptionOptions` is true.
   --
  } deriving stock (Show, Eq, Generic)
----------------------------------------------------------------------------
instance FromJSON SetExceptionBreakpointsArguments where
  parseJSON = genericParseJSONWithModifier
----------------------------------------------------------------------------
data ExceptionFilterOptions
  = ExceptionFilterOptions
  { exceptionFilterOptionsFilterId :: String
    -- ^ ID of an exception filter returned by the `exceptionBreakpointFilters`
    -- capability.
  , exceptionFilterOptionsCondition :: Maybe String
    -- ^ An expression for conditional exceptions.
    -- The exception breaks into the debugger if the result of the condition is
    -- true.
  } deriving stock (Show, Eq, Generic)
---------------------------------------------------------------------------
instance FromJSON ExceptionFilterOptions where
  parseJSON = genericParseJSONWithModifier
----------------------------------------------------------------------------
data ExceptionOptions
  = ExceptionOptions
  { exceptionOptionsPath :: Maybe [ExceptionPathSegment]
    -- ^
    -- A path that selects a single or multiple exceptions in a tree. If `path` is
    -- missing, the whole tree is selected.
    -- By convention the first segment of the path is a category that is used to
    -- group exceptions in the UI.
    --
  , exceptionOptionsBreakMode :: ExceptionBreakMode
    -- ^
    -- Condition when a thrown exception should result in a break.
    --
  } deriving stock (Show, Eq, Generic)
----------------------------------------------------------------------------
instance FromJSON ExceptionOptions where
  parseJSON = genericParseJSONWithModifier
----------------------------------------------------------------------------
data DataBreakpointInfoArguments
  = DataBreakpointInfoArguments
  { dataBreakpointInfoArgumentsVariablesReference :: Maybe Int
    -- ^
    -- Reference to the variable container if the data breakpoint is requested for
    -- a child of the container. The `variablesReference` must have been obtained
    -- in the current suspended state. See 'Lifetime of Object References' in the
    -- Overview section for details.
    --
  , dataBreakpointInfoArgumentsName :: Text
    -- ^
    -- The name of the variable's child to obtain data breakpoint information for.
    -- If `variablesReference` isn't specified, this can be an expression.
    --
  , dataBreakpointInfoArgumentsFrameId :: Maybe Int
    -- ^
    -- When `name` is an expression, evaluate it in the scope of this stack frame.
    -- If not specified, the expression is evaluated in the global scope. When
    -- `variablesReference` is specified, this property has no effect.
    --
  } deriving stock (Show, Eq, Generic)
----------------------------------------------------------------------------
instance FromJSON DataBreakpointInfoArguments where
  parseJSON = genericParseJSONWithModifier
----------------------------------------------------------------------------
data SetDataBreakpointsArguments
  = SetDataBreakpointsArguments
  { setDataBreakpointsArgumentsBreakpoints :: [DataBreakpoint]
    -- ^
    -- The contents of this array replaces all existing data breakpoints. An empty
    -- array clears all data breakpoints.
    --
  } deriving stock (Show, Eq)
----------------------------------------------------------------------------
data DataBreakpoint
  = DataBreakpoint
  { dataBreakpointDataId :: Text
    -- ^
    -- An id representing the data. This id is returned from the
    -- `dataBreakpointInfo` request.
    --
  , dataBreakpointAccessType :: Maybe DataBreakpointAccessType
    -- ^
    -- The access type of the data.
    --
  , condition :: Maybe Text
    -- ^
    -- An expression for conditional breakpoints.
    --
  , hitCondition :: Maybe Text
    -- ^
    -- An expression that controls how many hits of the breakpoint are ignored.
    -- The debug adapter is expected to interpret the expression as needed.
    --
  } deriving stock (Show, Eq)
----------------------------------------------------------------------------
data SetInstructionBreakpointsArguments
  = SetInstructionBreakpointsArguments
  { breakpoints :: [InstructionBreakpoint]
    -- ^
    -- The instruction references of the breakpoints
    --
  } deriving (Show, Eq)
----------------------------------------------------------------------------
data InstructionBreakpoint
  = InstructionBreakpoint
  { instructionBreakpointInstructionReference :: Text
    -- ^
    -- The instruction reference of the breakpoint.
    -- This should be a memory or instruction pointer reference from an
    -- `EvaluateResponse`, `Variable`, `StackFrame`, `GotoTarget`, or
    -- `Breakpoint`.
    --
  , instructionBreakpointOffset :: Maybe Int
    -- ^
    -- The offset from the instruction reference.
    -- This can be negative.
    --
  , instructionBreakpointCondition :: Maybe Text
    -- ^
    -- An expression for conditional breakpoints.
    -- It is only honored by a debug adapter if the corresponding capability
    -- `supportsConditionalBreakpoints` is true.
    --
  , instructionBreakpointHitCondition :: Maybe Text
    -- ^
    -- An expression that controls how many hits of the breakpoint are ignored.
    -- The debug adapter is expected to interpret the expression as needed.
    -- The attribute is only honored by a debug adapter if the corresponding
    -- capability `supportsHitConditionalBreakpoints` is true.
    --
  } deriving stock (Show, Eq)
----------------------------------------------------------------------------
data ContinueArguments
  = ContinueArguments
  { continueArgumentsThreadId :: Int
    -- ^
    -- Specifies the active thread. If the debug adapter supports single thread
    -- execution (see `supportsSingleThreadExecutionRequests`) and the argument
    -- `singleThread` is true, only the thread with this ID is resumed.
    --
  , continueArgumentsSingleThread :: Bool
    -- ^
    -- If this flag is true, execution is resumed only for the thread with given
    -- `threadId`.
    --
  } deriving stock (Show, Eq, Generic)
----------------------------------------------------------------------------
instance FromJSON ContinueArguments where
  parseJSON = genericParseJSONWithModifier
----------------------------------------------------------------------------
data NextArguments
  = NextArguments
  { nextArgumentsThreadId :: Int
   -- ^
   -- Specifies the thread for which to resume execution for one step (of the
   -- given granularity).
   --
  , nextArgumentsSingleThread :: Maybe Bool
   -- ^
   -- If this flag is true, all other suspended threads are not resumed.
   --
  , nextArgumentsGranularity :: Maybe SteppingGranularity
   -- ^
   -- Stepping granularity. If no granularity is specified, a granularity of
   -- `statement` is assumed.
   --
  } deriving stock (Show, Eq, Generic)
----------------------------------------------------------------------------
instance FromJSON NextArguments where
  parseJSON = genericParseJSONWithModifier
----------------------------------------------------------------------------
data StepInArguments
  = StepInArguments
  { stepInArgumentsThreadId :: Int
    -- ^
    -- Specifies the thread for which to resume execution for one step-into (of
    -- the given granularity).
    --
  , stepInArgumentsSingleThread :: Bool
    -- ^
    -- If this flag is true, all other suspended threads are not resumed.
    --
  , stepInArgumentsTargetId :: Maybe Int
    -- ^
    -- Id of the target to step into.
    --
  , stepInArgumentsGranularity :: Maybe SteppingGranularity
    -- ^
    -- Stepping granularity. If no granularity is specified, a granularity of
    -- `statement` is assumed.
    --
  } deriving stock (Show, Eq)
----------------------------------------------------------------------------
data StepOutArguments
  = StepOutArguments
  { stepOutArgumentsThreadId :: Int
    -- ^
    -- Specifies the thread for which to resume execution for one step-out (of the
    -- given granularity).
    --
  , stepOutArgumentsSingleThread :: Bool
    -- ^
    -- If this flag is true, all other suspended threads are not resumed.
    --
  , stepOutArgumentsGranularity :: Maybe SteppingGranularity
    -- ^
    -- Stepping granularity. If no granularity is specified, a granularity of
    -- `statement` is assumed.
    --
  } deriving stock (Show, Eq)
----------------------------------------------------------------------------
data StepBackArguments
  = StepBackArguments
  { stepBackArgumentsThreadId :: Int
    -- ^
    -- Specifies the thread for which to resume execution for one step backwards
    -- (of the given granularity).
    --
  , stepBackArgumentsSingleThread :: Bool
    -- ^
    -- If this flag is true, all other suspended threads are not resumed.
    --
  , stepBackArgumentsGranularity :: Maybe SteppingGranularity
    -- ^
    -- Stepping granularity to step. If no granularity is specified, a granularity
    -- of `statement` is assumed.
  } deriving stock (Show, Eq)
----------------------------------------------------------------------------
data SteppingGranularity
  = SteppingGranularityStatement
  | SteppingGranularityLine
  | SteppingGranularityInstruction
  deriving stock (Show, Eq, Generic)
----------------------------------------------------------------------------
instance FromJSON SteppingGranularity where
  parseJSON = genericParseJSONWithModifier
----------------------------------------------------------------------------
data ReverseContinueArguments
  = ReverseContinueArguments
  { reverseContinueArgumentsThreadId :: Int
   -- ^
   -- Specifies the active thread. If the debug adapter supports single thread
   -- execution (see `supportsSingleThreadExecutionRequests`) and the
   -- `singleThread` argument is true, only the thread with this ID is resumed.
   --
  , reverseContinueArgumentsSingleThread :: Bool
   -- ^
   -- If this flag is true, backward execution is resumed only for the thread
   -- with given `threadId`.
   --
  } deriving stock (Show, Eq, Generic)
----------------------------------------------------------------------------
instance FromJSON ReverseContinueArguments where
  parseJSON = genericParseJSONWithModifier
----------------------------------------------------------------------------
data RestartFrameArguments
  = RestartFrameArguments
  { restartFrameArgumentsFrameId :: Int
   -- ^
   -- Restart the stack frame identified by `frameId`. The `frameId` must have
   -- been obtained in the current suspended state. See 'Lifetime of Object
   -- References' in the Overview section for details.
   --
  } deriving stock (Show, Eq, Generic)
----------------------------------------------------------------------------
instance FromJSON RestartFrameArguments where
  parseJSON = genericParseJSONWithModifier
----------------------------------------------------------------------------
data GotoArguments
  = GotoArguments
  { gotoArgumentsThreadId :: Int
    -- ^
    -- Set the goto target for this thread.
    --
  , gotoArgumentsTargetId :: Int
    -- ^
    -- The location where the debuggee will continue to run.
    --
  } deriving stock (Show, Eq, Generic)
----------------------------------------------------------------------------
instance FromJSON GotoArguments where
  parseJSON = genericParseJSONWithModifier
----------------------------------------------------------------------------
data PauseArguments
  = PauseArguments
  { pauseArgumentsThreadId :: Int
    -- ^
    -- Pause execution for this thread.
    --
  } deriving stock (Show, Eq, Generic)
----------------------------------------------------------------------------
instance FromJSON PauseArguments where
  parseJSON = genericParseJSONWithModifier
----------------------------------------------------------------------------
data StackTraceArguments
  = StackTraceArguments
  { stackTraceArgumentsThreadId :: Int
    -- ^
    -- Retrieve the stacktrace for this thread.
    --
  , stackTraceArgumentsStartFrame :: Maybe Int
    -- ^
    -- The index of the first frame to return; if omitted frames start at 0.
    --
  , stackTraceArgumentsLevels :: Maybe Int
    -- ^
    -- The maximum number of frames to return. If levels is not specified or 0,
    -- all frames are returned.
    --
  , stackTraceArgumentsFormat :: Maybe StackFrameFormat
    -- ^
    -- Specifies details on how to format the stack frames.
    -- The attribute is only honored by a debug adapter if the corresponding
    -- capability `supportsValueFormattingOptions` is true.
    --
  } deriving stock (Show, Eq, Generic)
----------------------------------------------------------------------------
instance FromJSON StackTraceArguments where
  parseJSON = genericParseJSONWithModifier
----------------------------------------------------------------------------
data StackFrameFormat
  = StackFrameFormat
  { stackFrameFormatParameters :: Maybe Bool
    -- ^
    -- Displays parameters for the stack frame.
    --
  , stackFrameFormatParameterTypes :: Maybe Bool
    -- ^
    -- Displays the types of parameters for the stack frame.
    --
  , stackFrameFormatParameterNames :: Maybe Bool
    -- ^
    -- Displays the names of parameters for the stack frame.
    --
  , stackFrameFormatParameterValues :: Maybe Bool
    -- ^
    -- Displays the values of parameters for the stack frame.
    --
  , stackFrameFormatLine :: Maybe Bool
    -- ^
    -- Displays the line number of the stack frame.
    --
  , stackFrameFormatModule :: Maybe Bool
    -- ^
    -- Displays the module of the stack frame.
    --
  , stackFrameFormatIncludeAll :: Maybe Bool
    -- ^
    -- Includes all stack frames, including those the debug adapter might
    -- otherwise hide.
    --
  , stackFrameFormatHex :: Maybe Bool
    -- ^
    -- Display the value in hex.
    --
  } deriving stock (Show, Eq, Generic)
----------------------------------------------------------------------------
instance FromJSON StackFrameFormat where
  parseJSON = genericParseJSONWithModifier
----------------------------------------------------------------------------
data ScopesArguments
  = ScopesArguments
  { scopesArgumentsFrameId :: Int
    -- ^
    -- Retrieve the scopes for the stack frame identified by `frameId`. The
    -- `frameId` must have been obtained in the current suspended state. See
    -- 'Lifetime of Object References' in the Overview section for details.
    --
  } deriving stock (Show, Eq, Generic)
----------------------------------------------------------------------------
instance FromJSON ScopesArguments where
  parseJSON = genericParseJSONWithModifier
----------------------------------------------------------------------------
data VariablesFilter
  = VariablesFilterIndexed
  | VariablesFilterNamed
  deriving stock (Show, Eq, Generic)
----------------------------------------------------------------------------
instance FromJSON VariablesFilter where
  parseJSON = genericParseJSONWithModifier
----------------------------------------------------------------------------
data VariablesArguments
  = VariablesArguments
  { variablesArgumentsVariablesReference :: Int
    -- ^
    -- The variable for which to retrieve its children. The `variablesReference`
    -- must have been obtained in the current suspended state. See 'Lifetime of
    -- Object References' in the Overview section for details.
    --
  , variablesArgumentsFilter :: Maybe VariablesFilter
    -- ^
    -- Filter to limit the child variables to either named or indexed. If omitted,
    -- both types are fetched.
    -- Values: 'indexed', 'named'
    --
  , variablesArgumentsStart :: Maybe Int
    -- ^
    -- The index of the first variable to return; if omitted children start at 0.
    --
  , variablesArgumentsCount :: Maybe Int
    -- ^
    -- The number of variables to return. If count is missing or 0, all variables
    -- are returned.
    --
  , variablesArgumentsFormat :: Maybe ValueFormat
    -- ^
    -- Specifies details on how to format the Variable values.
    -- The attribute is only honored by a debug adapter if the corresponding
    -- capability `supportsValueFormattingOptions` is true.
    --
  } deriving stock (Show, Eq, Generic)
----------------------------------------------------------------------------
instance FromJSON VariablesArguments where
  parseJSON = genericParseJSONWithModifier
----------------------------------------------------------------------------
data ValueFormat
  = ValueFormat
  { valueFormatHex :: Maybe Bool
    -- ^
    -- Display the value in hex.
    --
  } deriving stock (Show, Eq, Generic)
----------------------------------------------------------------------------
instance FromJSON ValueFormat where
  parseJSON = genericParseJSONWithModifier
----------------------------------------------------------------------------
data SetVariableArguments
  = SetVariableArguments
  { setVariableArgumentsVariablesReference :: Int
    -- ^
    -- The reference of the variable container. The `variablesReference` must have
    -- been obtained in the current suspended state. See 'Lifetime of Object
    -- References' in the Overview section for details.
    --
  , setVariableArgumentsName :: Text
    -- ^
    -- The name of the variable in the container.
    --
  , setVariableArgumentsValue :: Text
    -- ^
    -- The value of the variable.
    --
  , setVariableArgumentsFormat :: Maybe ValueFormat
    -- ^
    -- Specifies details on how to format the response value.
    --
  } deriving stock (Show, Eq, Generic)
----------------------------------------------------------------------------
instance FromJSON SetVariableArguments where
  parseJSON = genericParseJSONWithModifier
----------------------------------------------------------------------------
data SourceArguments
  = SourceArguments
  { sourceArgumentsSource :: Maybe Source
    -- ^
    -- Specifies the source content to load. Either `source.path` or
    -- `source.sourceReference` must be specified.
    --
  , sourceArgumentsSourceReference :: Int
    -- ^
    -- The reference to the source. This is the same as `source.sourceReference`.
    -- This is provided for backward compatibility since old clients do not
    -- understand the `source` attribute.
    --
  } deriving stock (Show, Eq, Generic)
----------------------------------------------------------------------------
instance FromJSON SourceArguments where
  parseJSON = genericParseJSONWithModifier
----------------------------------------------------------------------------
newtype TerminateThreadsArguments
  = TerminateThreadsArguments
  { terminateThreadsArgumentsThreadIds :: [Int]
    -- ^
    -- Ids of threads to be terminated.
    --
  } deriving stock (Show, Eq, Generic)
----------------------------------------------------------------------------
instance FromJSON TerminateThreadsArguments where
  parseJSON = genericParseJSONWithModifier
----------------------------------------------------------------------------
data ModulesArguments
  = ModulesArguments
  { modulesArgumentsStartModule :: Maybe Int
    -- ^
    -- The index of the first module to return; if omitted modules start at 0.
    --
  , modulesArgumentsModuleCount :: Maybe Int
    -- ^
    -- The number of modules to return. If `moduleCount` is not specified or 0,
    -- all modules are returned.
    --
  } deriving stock (Show, Eq, Generic)
----------------------------------------------------------------------------
data LoadedSourcesArguments = LoadedSourcesArguments
  deriving stock (Show, Eq)
----------------------------------------------------------------------------
instance FromJSON LoadedSourcesArguments where
  parseJSON _ = pure LoadedSourcesArguments
----------------------------------------------------------------------------
data EvaluateArgumentsContext
  = EvaluateArgumentsContextWatch
  | EvaluateArgumentsContextRepl
  | EvaluateArgumentsContextHover
  | EvaluateArgumentsContextClipboard
  | EvaluateArgumentsContextVariable
  deriving stock (Show, Eq, Generic)
----------------------------------------------------------------------------
instance ToJSON EvaluateArgumentsContext where
  toJSON = genericToJSONWithModifier
----------------------------------------------------------------------------
instance FromJSON EvaluateArgumentsContext where
  parseJSON = genericParseJSONWithModifier
----------------------------------------------------------------------------
data EvaluateArguments
  = EvaluateArguments
  { evaluateArgumentsExpression :: Text
    -- ^
    -- The expression to evaluate.
    --
  , evaluateArgumentsFrameId:: Maybe Int
    -- ^
    -- Evaluate the expression in the scope of this stack frame. If not specified,
    -- the expression is evaluated in the global scope.
    --
  , evaluateArgumentsContext :: Maybe EvaluateArgumentsContext
    -- ^
    -- The context in which the evaluate request is used.
    -- Values:
    -- 'watch': evaluate is called from a watch view context.
    -- 'repl': evaluate is called from a REPL context.
    -- 'hover': evaluate is called to generate the debug hover contents.
    -- This value should only be used if the corresponding capability
    -- `supportsEvaluateForHovers` is true.
    -- 'clipboard': evaluate is called to generate clipboard contents.
    -- This value should only be used if the corresponding capability
    -- `supportsClipboardContext` is true.
    -- 'variables': evaluate is called from a variables view context.
    -- etc.
    --
  , evaluateArgumentsFormat :: Maybe ValueFormat
    -- ^
    -- Specifies details on how to format the result.
    -- The attribute is only honored by a debug adapter if the corresponding
    -- capability `supportsValueFormattingOptions` is true.
    --
  } deriving stock (Show, Eq, Generic)
----------------------------------------------------------------------------
instance FromJSON EvaluateArguments where
  parseJSON = genericParseJSONWithModifier
----------------------------------------------------------------------------
data SetExpressionArguments
  = SetExpressionArguments
  { setExpressionArgumentsExpression :: Text
    -- ^
    -- The l-value expression to assign to.
    --
  , setExpressionArgumentsValue :: Text
    -- ^
    -- The value expression to assign to the l-value expression.
    --
  , setExpressionArgumentsFrameId :: Maybe Int
    -- ^
    -- Evaluate the expressions in the scope of this stack frame. If not
    -- specified, the expressions are evaluated in the global scope.
    --
  , setExpressionArgumentsFormat :: Maybe ValueFormat
    -- ^
    -- Specifies how the resulting value should be formatted.
    --
  } deriving stock (Show, Eq, Generic)
----------------------------------------------------------------------------
instance FromJSON SetExpressionArguments where
  parseJSON = genericParseJSONWithModifier
----------------------------------------------------------------------------
data StepInTargetsArguments
  = StepInTargetsArguments
  { stepInTargetsArgumentsFrameId :: Int
    -- ^
    -- The stack frame for which to retrieve the possible step-in targets.
    --
  } deriving stock (Show, Eq, Generic)
----------------------------------------------------------------------------
instance FromJSON StepInTargetsArguments where
  parseJSON = genericParseJSONWithModifier
----------------------------------------------------------------------------
data GotoTargetsArguments
  = GotoTargetsArguments
  { gotoTargetsArgumentsSource :: Source
    -- ^
    -- The source location for which the goto targets are determined.
    --
  , gotoTargetsArgumentsLine :: Int
    -- ^
    -- The line location for which the goto targets are determined.
    --
  , gotoTargetsArgumentsColumn :: Maybe Int
    -- ^
    -- The position within `line` for which the goto targets are determined. It is
    -- measured in UTF-16 code units and the client capability `columnsStartAt1`
    -- determines whether it is 0- or 1-based.
    --
  } deriving stock (Show, Eq, Generic)
----------------------------------------------------------------------------
instance FromJSON GotoTargetsArguments where
  parseJSON = genericParseJSONWithModifier
----------------------------------------------------------------------------
data CompletionsArguments
  = CompletionsArguments
  { completionsArgumentsFrameId :: Maybe Int
    -- ^
    -- Returns completions in the scope of this stack frame. If not specified, the
    -- completions are returned for the global scope.
    --
  , completionsArgumentsText :: Text
    -- ^
    -- One or more source lines. Typically this is the text users have typed into
    -- the debug console before they asked for completion.
    --
  , completionsArgumentsColumn :: Int
    -- ^
    -- The position within `text` for which to determine the completion proposals.
    -- It is measured in UTF-16 code units and the client capability
    -- `columnsStartAt1` determines whether it is 0- or 1-based.
    --
  , completionsArgumentsLine :: Maybe Int
    -- ^
    -- A line for which to determine the completion proposals. If missing the
    -- first line of the text is assumed.
    --
  } deriving stock (Show, Eq, Generic)
----------------------------------------------------------------------------
instance FromJSON CompletionsArguments where
  parseJSON = genericParseJSONWithModifier
----------------------------------------------------------------------------
data ExceptionInfoArguments
  = ExceptionInfoArguments
  { exceptionInfoArgumentsThreadId :: Int
    -- ^
    -- Thread for which exception information should be retrieved.
    --
  } deriving stock (Show, Eq, Generic)
----------------------------------------------------------------------------
instance FromJSON ExceptionInfoArguments where
  parseJSON = genericParseJSONWithModifier
----------------------------------------------------------------------------
data ReadMemoryArguments
  = ReadMemoryArguments
  { readMemoryArgumentsmemoryReference :: Text
    -- ^
    -- Memory reference to the base location from which data should be read.
    --
  , readMemoryArgumentsOffset :: Maybe Int
    -- ^
    -- Offset (in bytes) to be applied to the reference location before reading
    -- data. Can be negative.
    --
  , readMemoryArgumentsCount :: Int
    -- ^
    -- Number of bytes to read at the specified location and offset.
    --
  } deriving stock (Show, Eq, Generic)
----------------------------------------------------------------------------
instance FromJSON ReadMemoryArguments where
  parseJSON = genericParseJSONWithModifier
----------------------------------------------------------------------------
data WriteMemoryArguments
  = WriteMemoryArguments
  { writeMemoryMemoryReference :: Text
    -- ^
    -- Memory reference to the base location to which data should be written.
    --
  , writeMemoryArgumentsOffset :: Maybe Int
    -- ^
    -- Offset (in bytes) to be applied to the reference location before writing
    -- data. Can be negative.
    --
  , writeMemoryArgumentsAllowPartial :: Bool
    -- ^
    -- Property to control partial writes. If true, the debug adapter should
    -- attempt to write memory even if the entire memory region is not writable.
    -- In such a case the debug adapter should stop after hitting the first byte
    -- of memory that cannot be written and return the number of bytes written in
    -- the response via the `offset` and `bytesWritten` properties.
    -- If false or missing, a debug adapter should attempt to verify the region is
    -- writable before writing, and fail the response if it is not.
    --
  , writeMemoryArgumentsData :: Text
    -- ^
    -- Bytes to write, encoded using base64.
    --
  } deriving stock (Show, Eq, Generic)
----------------------------------------------------------------------------
instance FromJSON WriteMemoryArguments where
  parseJSON = genericParseJSONWithModifier
----------------------------------------------------------------------------
data DisassembleArguments
  = DisassembleArguments
  { disassembleArgumentsMemoryReference :: Text
    -- ^
    -- Memory reference to the base location containing the instructions to
    -- disassemble.
    --
  , disassembleArgumentsOffset :: Maybe Int
    -- ^
    -- Offset (in bytes) to be applied to the reference location before
    -- disassembling. Can be negative.
    --
  , disassembleArgumentsInstructionOffset :: Maybe Int
    -- ^
    -- Offset (in instructions) to be applied after the byte offset (if any)
    -- before disassembling. Can be negative.
    --
  , disassembleArgumentsInstructionCount :: Int
    -- ^
    -- Number of instructions to disassemble starting at the specified location
    -- and offset.
    -- An adapter must return exactly this number of instructions - any
    -- unavailable instructions should be replaced with an implementation-defined
    -- 'invalid instruction' value.
    --
  , disassembleArgumentsResolveSymbols :: Bool
    -- ^
    -- If true, the adapter should attempt to resolve memory addresses and other
    -- values to symbolic names.
    --
   } deriving stock (Show, Eq, Generic)
----------------------------------------------------------------------------
instance FromJSON DisassembleArguments where
  parseJSON = genericParseJSONWithModifier
----------------------------------------------------------------------------
-- | A ColumnDescriptor specifies what module attribute to show in a column of the modules view, how to format it,
-- and what the column’s label should be.
-- It is only used if the underlying UI actually supports this level of customization.
--
data ColumnDescriptor
  = ColumnDescriptor
  { columnDescriptorAttributeName :: String
    -- ^
    -- Name of the attribute rendered in this column.
    --
  , columnDescriptorLabel :: String
    -- ^
    -- Header UI label of column.
    --
  , columnDescriptorFormat :: Maybe String
    -- ^
    -- Format to use for the rendered values in this column. TBD how the format
    -- strings looks like.
    --
  , columnDescriptorType :: Maybe ColumnDescriptorType
    -- ^
    -- Datatype of values in this column. Defaults to `string` if not specified.
    -- Values: 'string', 'number', 'boolean', 'unixTimestampUTC'
    --
  , columnDescriptorWidth :: Maybe Int
    -- ^
    -- Width of this column in characters (hint only).
    --
  } deriving stock (Show, Eq, Generic)
----------------------------------------------------------------------------
instance ToJSON ColumnDescriptor where
  toJSON = genericToJSONWithModifier
----------------------------------------------------------------------------
data ColumnDescriptorType
  = ColumnDescriptorTypeString
  | ColumnDescriptorTypeInt
  | ColumnDescriptorTypeBool
  | ColumnDescriptorTypeUTCTime UTCTime
  deriving stock (Show, Eq)
----------------------------------------------------------------------------
instance ToJSON ColumnDescriptorType where
  toJSON (ColumnDescriptorTypeUTCTime utcTime) = toJSON utcTime
  toJSON typ = enumToLowerCamel (Proxy @ColumnDescriptorType) typ
----------------------------------------------------------------------------
-- | An ExceptionPathSegment represents a segment in a path that is used to match leafs or nodes in a tree of exceptions.
-- If a segment consists of more than one name, it matches the names provided if negate is false or missing, or it matches anything except the names provided if negate is true.
--
data ExceptionPathSegment
  = ExceptionPathSegment
  { exceptionPathSegmentNegate :: Maybe Bool
  , exceptionPathSegmentNames  :: [String]
  } deriving stock (Show, Eq, Generic)
----------------------------------------------------------------------------
instance FromJSON ExceptionPathSegment where
  parseJSON = genericParseJSONWithModifier
----------------------------------------------------------------------------
data ModulesViewDescriptor
  = ModulesViewDescriptor
  { modulesViewDescriptorColumns :: [ColumnDescriptor]
  } deriving stock (Show, Eq)
----------------------------------------------------------------------------
data CompletionItemType
  = CompletionItemTypeMethod
  | CompletionItemTypeFunction
  | CompletionItemTypeConstructor
  | CompletionItemTypeField
  | CompletionItemTypeVariable
  | CompletionItemTypeClass
  | CompletionItemTypeInterface
  | CompletionItemTypeModule
  | CompletionItemTypeProperty
  | CompletionItemTypeUnit
  | CompletionItemTypeValue
  | CompletionItemTypeEnum
  | CompletionItemTypeKeyword
  | CompletionItemTypeSnippet
  | CompletionItemTypeText
  | CompletionItemTypeColor
  | CompletionItemTypeFile
  | CompletionItemTypeReference
  | CompletionItemTypeCustomcolor
  deriving stock (Show, Eq, Generic)
----------------------------------------------------------------------------
-- | An ExceptionBreakpointsFilter is shown in the UI as an filter option for configuring how exceptions are dealt with.
data ExceptionBreakpointsFilter
  = ExceptionBreakpointsFilter
  { exceptionBreakpointsFilterFilter :: Text
    -- ^
    -- The internal ID of the filter option. This value is passed to the
    -- `setExceptionBreakpoints` request.
    --
  , exceptionBreakpointsFilterLabel :: Text
    -- ^
    -- The name of the filter option. This is shown in the UI.
    --
  , exceptionBreakpointsFilterDescription :: Maybe Text
    -- ^
    -- A help text providing additional information about the exception filter.
    -- This string is typically shown as a hover and can be translated.
    --
  , exceptionBreakpointsFilterDefault :: Maybe Bool
    -- ^
    -- Initial value of the filter option. If not specified a value false is
    -- assumed.
    --
  , exceptionBreakpointsFilterSupportsCondition :: Maybe Bool
    -- ^
    -- Controls whether a condition can be specified for this filter option. If
    -- false or missing, a condition can not be set.
    --
  , exceptionBreakpointsFilterConditionDescription :: Maybe Text
    -- ^
    -- A help text providing information about the condition. This string is shown
    -- as the placeholder text for a text box and can be translated.
    --
  } deriving stock (Show, Eq)
----------------------------------------------------------------------------
instance ToJSON ExceptionBreakpointsFilter where
  toJSON ExceptionBreakpointsFilter{..}
    = object
    [ "filter"               .= exceptionBreakpointsFilterFilter
    , "label"                .= exceptionBreakpointsFilterLabel
    , "description"          .= exceptionBreakpointsFilterDescription
    , "default"              .= exceptionBreakpointsFilterDefault
    , "supportsCondition"    .= exceptionBreakpointsFilterSupportsCondition
    , "conditionDescription" .= exceptionBreakpointsFilterConditionDescription
    ]
----------------------------------------------------------------------------
data ConfigurationDoneArguments = ConfigurationDoneArguments
  deriving stock (Show, Eq)
----------------------------------------------------------------------------
instance FromJSON ConfigurationDoneArguments where
   parseJSON _ = pure ConfigurationDoneArguments
----------------------------------------------------------------------------
data ThreadsArguments = ThreadsArguments
  deriving stock (Show, Eq)
----------------------------------------------------------------------------
instance FromJSON ThreadsArguments where
   parseJSON _ = pure ThreadsArguments
----------------------------------------------------------------------------
data Level = DEBUG | INFO | WARN | ERROR
  deriving (Show, Eq)
----------------------------------------------------------------------------
data DebugStatus = SENT | RECEIVED
  deriving (Show, Eq)
----------------------------------------------------------------------------
