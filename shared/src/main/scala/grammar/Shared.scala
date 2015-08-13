package grammar

trait StringLike {
  def string: String
}

/**
  * Created by Mikael on 10.08.2015.
  */
object Shared {
  implicit def convertStringLike(t: StringLike): String = t.string

  val ACTION = "action"
  val WHAT = "what"
  val PROBLEM_ID =  "problemId"
  val EXERCISE_ID =  "exerciseId"

  val ALL_GRAMMARS_ID = "all_grammars"

  /* In-Server events */
  val DELETE_PROBLEM = "delete_problem"
  val ADMIN_MODE = "adminMode"
  val HELLO = "hello"
  val ABORT_OPS = "abortOps"
  val DO_UPDATE_CODE = "doUpdateCode"
  val GET_EXERCISE_TYPES = "getExerciseTypes"
  val GET_PROBLEM_LIST = "getProblemList"
  val LOAD_EXERCISE = "loadExercise"
  val DO_CHECK = "doCheck"
  val CHECK_LL1 = "checkLL1"
  val CHECK_AMBIGUITY = "checkAmbiguity"
  val NORMALIZE = "normalize"
  val GET_HINTS = "getHints"
  val GET_HELP = "getHelp"
  val SOLVE = "solve"
  val SAVE_GRAMMAR = "saveGrammar"
  object SAVE {
    val title = "title"
    val description = "description"
    val reference = "reference"
    val initial = "initial"
    val word = "word"
    val usecases = "usecases"
  }
  val ALL_USE_CASES = "all_usecases"
  val NEW_PROBLEM_ID = "new_problem_id"

  /* Out-server events */
  val CONSOLE = "console"
  val HELP_MSG = "helpmsg"
  val DISABLE_EVENTS = "disableEvents"
  val ENABLE_EVENTS = "enableEvents"
  val EXERCISE_TYPES = "exerciseTypes"
  val PROBLEMS = "problems"
  val NOTIFICATION = "notification"
  object EXERCISE_DESC extends StringLike {
    val string = "exerciseDesc"
    def intro = "intro"
    def desc = "desc"
    def reference = "reference"
    /** initial grammar */
    val grammar = "grammar"
  }

  val ENTER_ADMIN_MODE = "EnterAdminMode"
  val REJECT_ADMIN_ACCESS = "RejectAdminAccess"
  val FULL_SOLUTION = "fullsolution"
  val REPLACE_GRAMMAR = "replace_grammar"

  object KIND {
    val key = "kind"
    def apply(value: String) = key -> value
    def ->(value: String) = this(value)
    lazy val console = this(CONSOLE)
  }
  object level {
    private val key = "level"
    lazy val log = key -> "log"
    lazy val error = key -> "error"
  }
  val MESSAGE = "message"

 }
