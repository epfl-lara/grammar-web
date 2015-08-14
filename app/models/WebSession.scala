package models

import java.io.File
import java.util.concurrent.Executors

import akka.actor._
import grammar.CFGrammar.{ Nonterminal, Terminal }
import grammar.Shared._
import grammar._
import grammar.exercises.ExerciseType.ExType
import grammar.exercises.{ ExerciseType, _ }
import parsing.CYKParser
import play.api.Play.current
import play.api._
import play.api.libs.iteratee._
import play.api.libs.json.Json._
import play.api.libs.json.Writes._
import play.api.libs.json._
import play.twirl.api.HtmlFormat

import scala.collection.mutable.{ ArrayBuffer, ListBuffer }
import scala.concurrent._
import scala.util.{ Failure, Success }

///import grammar.CNFConverter

object grammarDB {
  var db = GrammarDatabase.readGrammarDatabase(
    Play.getFile("/public/resources/GrammarDatabase.xml"),
    Play.getFile("/public/resources/").getAbsolutePath())
}

object Guid {
  private var id: Long = 0

  def getNextId = {
    id = id + 1
    id
  }
}

object AdminPassword {
  /**
   * Reset the password here.
   * TODO: we can use a SHA or Md5 hash here.
   */
  def checkPassword(pass: String) = {
    if (pass == "jiffy")
      true
    else false
  }
}

class WebSession(remoteIP: String) extends Actor {

  import Protocol._
  import Shared._

  //creating a thread pool for futures
  implicit val ec = ExecutionContext.fromExecutor(Executors.newFixedThreadPool(2))

  val eventLogger = {
    val filename = new java.text.SimpleDateFormat("yyyy-MM-dd-HH-mm-ss").format(new java.util.Date())
    val newfile = new File("logs/" + filename + ".log")
    newfile.createNewFile()
    new java.io.PrintWriter(newfile)
  }

  val (enumerator, channel) = Concurrent.broadcast[JsValue]

  def pushMessage(v: JsValue) = channel.push(v)

  def clientLog(msg: String)(implicit msgid: Long) = {
    val timestamp = new java.text.SimpleDateFormat("dd-HH-mm-ss").format(new java.util.Date())
    eventLogger.println("[>][" + msgid + "][" + timestamp + "] " + msg)
    eventLogger.flush()
    pushMessage(toJson(Map(KIND.console, level.log, MESSAGE -> msg)))
  }

  def clientError(msg: String)(implicit msgid: Long) = {
    val timestamp = new java.text.SimpleDateFormat("dd-HH-mm-ss").format(new java.util.Date())
    eventLogger.println("[>][" + msgid + "][" + timestamp + "] " + msg)
    eventLogger.flush()
    pushMessage(toJson(Map(KIND.console, level.error, MESSAGE -> msg)))
  }

  def event(kind: String, data: Map[String, JsValue]) = {
    pushMessage(toJson(Map(KIND.key -> toJson(kind)) ++ data))
  }

  //operation list
  sealed abstract class OpRes

  case class Last(str: Map[String, String]) extends OpRes

  case class Partial(str: Map[String, String], nextPart: Future[OpRes]) extends OpRes

  //a list operation futures
  var currentOp: Option[(Future[OpRes], GlobalContext)] = None

  def recordFuture(opfuture: Future[OpRes], opctx: GlobalContext, extype: Option[ExerciseType.ExType])(implicit msgid: Long) {

    currentOp = Some((opfuture, opctx))
    //register a call-back
    opfuture onComplete {
      case Success(opRes) if (currentOp.isDefined && currentOp.get._1 == opfuture) =>
        opRes match {
          case Last(resmap) =>
            //send the message to the client and complete the operation
            event(FEEDBACK, resmap.map { case (k, v) => (k, toJson(v)) })
            //clientLog(resstr)
            extype match {
              case Some(ExerciseType.GrammarEx) =>
                enableEvents(List(DO_CHECK, GET_HINTS))
              case Some(_) =>
                enableEvents(List(DO_CHECK))
              case _ =>
                //allow every thing here
                enableEvents(List(DO_CHECK, GET_HINTS))
            }
          case Partial(partRes, nextPart) =>
            //send partial result to the client and continue the next operation
            event(FEEDBACK, partRes.map { case (k, v) => (k, toJson(v)) })
            recordFuture(nextPart, opctx, extype)
        }
      //else do nothing            
      case Failure(msg) if (currentOp.isDefined && currentOp.get._1 == opfuture) =>
        clientLog("Operation aborted abnormally")
        //print the exception to the console
        println(msg)

      case _ =>
        //for debugging 
        println(s"operation $msgid terminated")
      //Here, the currentop has been superseeded by another operation 
      //or has been aborted. so ignore the result        
    }
  }

  def disableEvents(events: List[String]) {
    val emptymsg = toJson("")
    event(DISABLE_EVENTS, events.map(_ -> emptymsg).toMap)
  }

  def enableEvents(events: List[String]) {
    val emptymsg = toJson("")
    event(ENABLE_EVENTS, events.map(_ -> emptymsg).toMap)
  }

  //keep track of the mode 
  var adminMode = false

  def getGlobalContext = new GlobalContext()

  def receive = {
    case Init =>
      sender ! InitSuccess(enumerator)

    case FromClient(msg) =>
      //create a unique id for message
      implicit val msgid = Guid.getNextId
      try {
        val timestamp = new java.text.SimpleDateFormat("dd-HH-mm-ss").format(new java.util.Date())
        eventLogger.println("[<][" + msgid + "][" + timestamp + "] " + msg)
        eventLogger.flush()

        (msg \ "action").as[String] match {
          case HELLO =>
            clientLog("Welcome!")

          case ADMIN_MODE =>
            if (AdminPassword.checkPassword((msg \ "password").as[String])) {
              adminMode = true
              event(ENTER_ADMIN_MODE,
                Map(ALL_USE_CASES -> toJson(getAllUseCases),
                  NEW_PROBLEM_ID -> toJson(grammarDB.db.grammarEntries.map(_.id).max + 1)))
            } else
              event(REJECT_ADMIN_ACCESS, Map())

          case ABORT_OPS =>
            //abort the current operation
            currentOp match {
              case Some((_, opctx)) =>
                opctx.abort = true
              case _ => ;
            }
            //clear the currentOps
            currentOp = None
            //enable disabled events. However, do not enable more than allowed by the context
            val exid = (msg \ "exerciseId").as[String]
            val exType = ExerciseType.getExType(exid)
            exType match {
              case Some(ExerciseType.GrammarEx) =>
                enableEvents(List(GET_HINTS, DO_CHECK))
              case Some(_) => enableEvents(List(DO_CHECK))
              case None =>
                //here, enable all to be safe
                enableEvents(List(GET_HINTS, DO_CHECK))
            }

          case DO_UPDATE_CODE =>
            ;
          /*val rules = grammar.split("\n").toList
            //try to parse the grammar (syntax errors will be displayed to the console)            
            //it is reserved for newly created symbols
            val (bnf, errstr) = (new GrammarParser()).parseGrammar(rules)
            if (!bnf.isDefined)
              clientLog("Parse Error: " + errstr)*/
          case GET_EXERCISE_TYPES =>
            //read all exercises and send their names and ids to the clients
            case class ReturnResult(key: String, title: String)
            val data = (ExerciseType.allExercises ++ (Admin.moreExerciseTypes)).zipWithIndex.map(exType =>
              exType._2.toString -> toJson(Map("key" -> exType._1.key, "title" -> exType._1.toString))).toMap
            event(EXERCISE_TYPES, data)

          case GET_PROBLEM_LIST =>
            //read all problems in the grammar database that suit the exercise selected
            //and send their names and ids to the clients
            val exid = (msg \ EXERCISE_ID).as[String]
            val exType = ExerciseType.getExType(exid).orElse(Admin.extractExType(exid))
            if (exType.isDefined) {
              val grammarEntries = grammarDB.db.entriesForExercise(exType.get)
              //send all grammarEntries
              val data = generateProblemList(grammarEntries).map { case (k, v) => k -> toJson(v) }.toMap
              event(PROBLEMS, data)
              // enable or disable buttons
              exType.get match {
                case ExerciseType.GrammarEx => enableEvents(List(GET_HINTS, NORMALIZE))
                case _ => disableEvents(List(GET_HINTS, NORMALIZE))
              }
            } else {

              //log error message
              clientLog("Exercise with id: " + exid + " does not exist")
            }

          case LOAD_EXERCISE =>
            val pid = (msg \ PROBLEM_ID).as[String].toInt
            val data = grammarDB.db.grammarEntries.find(_.id == pid) match {
              case None =>
                Map(EXERCISE_DESC.desc -> toJson("There is no grammar in the database with the given id: " + pid))
              case Some(gentry) =>
                val exid = (msg \ EXERCISE_ID).as[String]
                val extype = ExerciseType.getExType(exid).orElse(Admin.extractExType(exid))
                //clientLog("Exercise ID: "+exid+" grammar: "+gentry.reference)
                if (extype.isDefined) {
                  val (problemData, initialGrammar) = generateProblemStatement(gentry, extype.get)
                  val data = problemData ++
                    (if (adminMode) Map(
                      ALL_USE_CASES -> toJson(getAllUseCases),
                      SAVE.usecases -> toJson(gentry.usecases.mkString("\n")),
                      SAVE.reference -> toJson(gentry.refGrammar.toString),
                      SAVE.title -> toJson(gentry.name),
                      SAVE.description -> toJson(gentry.desc)) ++
                      (gentry.initGrammar match {
                        case Some(init) => Map(SAVE.initial -> toJson(init.toString))
                        case None => Nil
                      }) ++
                      (gentry.word match {
                        case Some(word) => Map(SAVE.word -> toJson(word.map(_.toString).mkString(" ")))
                        case None => Nil
                      })
                    else Nil)
                  event(EXERCISE_DESC, data)
                  //some problems have an initial answer that the users have to refine 
                  if (initialGrammar.isDefined) {
                    val data = Map(EXERCISE_DESC.grammar -> toJson(initialGrammar.get.toString))
                    event(REPLACE_GRAMMAR, data)
                  }
                } else
                  //log error message
                  clientLog("Exercise with id: " + exid + " does not exist")
            }
          case DELETE_PROBLEM =>
            if (adminMode) {
              val pid = (msg \ PROBLEM_ID).as[String].toInt
              clientLog(s"Problem #$pid '${grammarDB.db.grammarEntries.find(_.id == pid).map(_.name).getOrElse("")}' deleted")
              grammarDB.db = grammarDB.db.deleted(pid)
              GrammarDatabase.writeGrammarDatabase(grammarDB.db)
            }
          case SAVE_GRAMMAR =>
            if (adminMode) {
              val pid = (msg \ PROBLEM_ID).as[String].toInt
              val what = (msg \ WHAT).as[String].split(",")
              val updatedFields = ArrayBuffer[String]()
              val gentry = grammarDB.db.grammarEntries.find(_.id == pid).getOrElse({
                updatedFields += "New exercise created with id " + pid
                val res = new GrammarEntry(
                  id = pid,
                  name = "",
                  desc = "",
                  reference = null,
                  word = null,
                  initGrammar = None,
                  usecases = Array("all"),
                  referenceFile = None,
                  initialFile = None)
                res
              })
              val updated_gentry = (gentry /: what) {
                case (g, SAVE.title) =>
                  val newTitle = (msg \ SAVE.title).as[String]
                  updatedFields += (s"Title '$newTitle' saved." + (if (g.name != "") " Previous value:\n" + g.name else ""))
                  g.copy(name = newTitle)
                case (g, SAVE.description) =>
                  updatedFields += ("Description saved." + (if (g.desc != "") " Previous value:\n" + g.desc else ""))
                  g.copy(desc = (msg \ SAVE.description).as[String])
                case (g, SAVE.reference) =>
                  val newReference = (msg \ SAVE.reference).as[String]
                  val (referenceOpt, error) = (new GrammarParser).parseGrammarContent(newReference)
                  referenceOpt match {
                    case None =>
                      clientLog("The grammar cannot parse. " + error); g
                    case Some(reference) =>
                      if (newReference != reference) {
                        updatedFields += ("Reference grammar for " + g.name + " saved")
                        g.copy(reference = reference).setToExportReference()
                      } else g
                  }
                case (g, SAVE.word) =>
                  val parseWord = (msg \ SAVE.word).as[String]
                  if (parseWord == "") {
                    g.copy(word = None)
                  } else {
                    ExerciseType.parseWord(List(parseWord), g.reference) match {
                      case Left(error) =>
                        clientLog("The given word cannot parse. " + error); g
                      case Right(words) =>
                        updatedFields += (s"Word '$parseWord' for " + g.name + " saved")
                        g.copy(word = Some(words))
                    }
                  }
                case (g, SAVE.initial) =>
                  val newInitGrammar = (msg \ SAVE.initial).as[String]
                  if (newInitGrammar == "") {
                    g.copy(initGrammar = None).setToExportInitGrammar()
                  } else {
                    val (initialOpt, error) = (new GrammarParser).parseGrammarContent(newInitGrammar)
                    initialOpt match {
                      case None =>
                        clientLog("The grammar cannot parse. " + error); g
                      case Some(initial) =>
                        updatedFields += ("Initial grammar for " + g.name + " saved")
                        g.copy(initGrammar = Some(initial)).setToExportInitGrammar()
                    }
                  }
                case (g, SAVE.usecases) =>
                  val newUseCases = (msg \ SAVE.usecases).as[String]
                  val prevUseCases = g.usecases.mkString(",")
                  updatedFields += (s"Use cases '$newUseCases' saved." + (if (newUseCases != prevUseCases) " Previous value:\n" + prevUseCases else ""))
                  g.copy(usecases = newUseCases.split(","))
                case (g, _) => g
              }
              grammarDB.db = grammarDB.db.updated(updated_gentry)
              GrammarDatabase.writeGrammarDatabase(grammarDB.db)
              val toTell = updatedFields mkString "\n"
              if (toTell != "") clientLog(toTell)
            }
          case DO_CHECK =>
            val pid = (msg \ "problemId").as[String].toInt
            grammarDB.db.grammarEntries.find(_.id == pid) match {
              case None =>
                clientLog("There is no grammar in the database with the given id: " + pid)
              case Some(gentry) =>
                val exid = (msg \ "exerciseId").as[String]
                val extype = ExerciseType.getExType(exid)
                if (extype.isDefined) {
                  val userAnswer = (msg \ "code").as[String]
                  //create a future for the operation and add it to the futures list
                  val opctx = getGlobalContext
                  val checkFuture = Future {
                    checkSolution(gentry, extype.get, userAnswer)(opctx)
                  }
                  recordFuture(checkFuture, opctx, extype)
                  //disable check and hints event, leave 'normalize' untouched              
                  disableEvents(List(GET_HINTS, DO_CHECK))

                } else
                  //log error message
                  clientLog("Exercise with id: " + exid + " does not exist")
            }

          case CHECK_LL1 =>
            val grammar = (msg \ "code").as[String]
            val rules = grammar.split("\n").toList
            //try to parse the grammar (syntax errors will be displayed in the client console)
            val (bnfGrammar, errstr) = (new GrammarParser()).parseGrammar(rules)
            if (!bnfGrammar.isDefined)
              clientLog("Parse Error:" + errstr)
            else {
              //check for ll1 property              
              clientLog(checkLL1(bnfGrammar.get).get)
            }

          case CHECK_AMBIGUITY =>
            val grammar = (msg \ "code").as[String]
            val rules = grammar.split("\n").toList
            //try to parse the grammar (syntax errors will be displayed in the client console)
            val (bnfGrammar, errstr) = (new GrammarParser()).parseGrammar(rules)
            if (!bnfGrammar.isDefined)
              clientLog("Parse Error:" + errstr)
            else {
              clientLog(checkAmbiguity(bnfGrammar.get)(getGlobalContext))
            }

          case NORMALIZE =>
            val grammar = (msg \ "code").as[String]
            val rules = grammar.split("\n").toList
            //try to parse the grammar (syntax errors will be displayed in the client console)
            val (bnfGrammar, errstr) = (new GrammarParser()).parseGrammar(rules)
            if (!bnfGrammar.isDefined)
              clientLog("Parse Error:" + errstr)
            else {
              //convert the grammar to cnf form and then reconvert              
              val normalGrammar = CFGrammar.prettyPrint(bnfGrammar.get.cfGrammar.fromCNF)
              val data = Map("grammar" -> toJson(normalGrammar.toString))
              event(REPLACE_GRAMMAR, data)
            }

          case GET_HINTS =>
            val pid = (msg \ "problemId").as[String].toInt
            val exercise = grammarDB.db.grammarEntries.find(_.id == pid)
            exercise match {
              case None =>
                clientLog("There is no exercise with the given id")
              case Some(ex) =>
                val grammar = (msg \ "code").as[String]
                val rules = grammar.split("\n").toList
                //try to parse the grammar (syntax errors will be displayed in the console)
                val (bnfGrammar, errstr) = (new GrammarParser()).parseGrammar(rules)
                if (!bnfGrammar.isDefined)
                  clientLog("Parse Error:" + errstr)
                else {
                  //create a future for the operation and add it to the futures list
                  val opctx = getGlobalContext
                  val hintFuture = Future {
                    provideHints(ex, bnfGrammar.get)(opctx)
                  }
                  recordFuture(hintFuture, opctx, None)
                  //disable hints and do-check, leave normalized untouched
                  disableEvents(List(GET_HINTS, DO_CHECK))
                }
            }

          case GET_HELP =>
            val exid = (msg \ "exerciseId").as[String]
            val extype = ExerciseType.getExType(exid)
            if (extype.isDefined) {
              val data = Map(MESSAGE -> toJson(helpMesage(extype.get)))
              event(HELP_MSG, data)
            } else
              //log error message
              clientLog("Exercise with id: " + exid + " does not exist")

          case SOLVE =>
            if (!adminMode) {
              clientLog("'Solve' operation can be invoked only in admin mode.")
            } else {
              val pid = (msg \ "problemId").as[String].toInt
              grammarDB.db.grammarEntries.find(_.id == pid) match {
                case None =>
                  clientLog("There is no grammar in the database with the selected id: " + pid)
                case Some(gentry) =>
                  val exid = (msg \ "exerciseId").as[String]
                  val extype = ExerciseType.getExType(exid)
                  if (extype.isDefined) {
                    val userAnswer = (msg \ "code").as[String]
                    //get the solution for the exercise and dump it 
                    val sol = getSolution(gentry, extype.get, userAnswer)
                    if (sol.isDefined) {
                      val data = Map(SOLUTION -> toJson(sol.get))
                      event(FULL_SOLUTION, data)
                    } else
                      clientLog("Cannot solve the problem.")
                  } else
                    //log error message
                    clientLog("Exercise with id: " + exid + " does not exist")
              }
            }

          case _ =>
            clientError("Error: Unhandled client event " + msg)
        }
      } catch {
        case t: Throwable =>
          t.printStackTrace()
          clientError("Could not process event: " + t.getMessage)
      }

    case Quit =>
      //close the log file here
      eventLogger.close()

    case msg =>
      clientError("Unknown message: " + msg)(0)
  }

  private def getAllUseCases: String = {
    (("all;All except '" + ExerciseType.ProgLangEx + "'") ::
      ("nogrammar;All except '" + ExerciseType.GrammarEx + "' and '" + ExerciseType.ProgLangEx + "'") ::
      ExerciseType.allExercises.map(e => e.key + ";" + e.toString)).mkString("\n")
  }

  protected object Admin {
    def moreExerciseTypes: Iterable[ExType] = {
      if (adminMode) Some(ExerciseType.AllGrammarsEx) else None
    }
    def extractExType(exid: String): Option[ExType] = {
      if (adminMode) ExerciseType.ExType.unapply(exid) else None
    }
  }

  import grammar._
  import BNFConverter._
  import CFGrammar._
  import EBNFGrammar._
  import clients._
  import equivalence._
  import generators.SizeBasedRandomAccessGenerator
  import grammar.exercises._
  import ExerciseType._
  import grammar.utils._
  import parsing._
  import repair.RepairResult._
  import repair._

  //minimum length of the word that has to be derived
  val minWordLength = 5
  val maxWordLength = 10
  val maxTries = 10

  //TODO: this is not thread safe, make this thread safe by extracting the string from
  //the problem statement
  var wordForDerivation: Option[Word] = None

  type ProblemID = String
  type ProblemName = String

  def generateProblemList(gentries: Seq[GrammarEntry]): Seq[(ProblemID, ProblemName)] = {
    //track the number of problems with the same name
    var seenNames = Set[String]()
    def getUniqueName(name: String, index: Int = 1): String = {
      val nname = if (index == 1)
        name
      else
        name + " " + index
      if (seenNames.contains(nname)) {
        getUniqueName(name, index + 1)
      } else nname
    }
    gentries.map(ge => {
      val newname = getUniqueName(ge.name)
      seenNames += newname
      ge.id.toString -> newname
    })
  }

  def helpMesage(ex: ExerciseType.ExType) = ex match {
    case DerivationEx =>
      "<ul><li>A leftmost derivation should have the start symbol on the first line</li>" +
        "<li>Each successive line, referred to as a step of the leftmost derivation, should be a sentential form " +
        "which is a sequence of nonterminals or terminals separated by whitespace. Terminals should not be enclosed within single quotes</li>" +
        "<li>Every step should be obtainable from the previous step by replacing the leftmost nonterminal by one of its productions</li>" +
        "<li>The last step of the derivation should be the string that has to be derived</li></ul>"

    case GrammarEx | CNFEx | GNFEx | ProgLangEx =>
      "<ul>Every line of the input should be a valid production in extended Backus-Naur form" +
        "<li> A production is of the form &lt;Nonterminal&gt; ::= &lt;Rightside&gt; (you can also use -> instead of ::= )</li>" +
        "<li>The left side of the first production is considered as the start symbol</li>" +
        "<li>Every symbol that does not appear on the left side of a production is considered a terminal</li>" +
        "<li>A &lt;Nonterminal&gt; is a sequence of alpha-numeric characters and underscore (_), that starts with an alphabet</li>" +
        "<li>A terminal is any contiguous sequence of characters, other than white-spaces and single quotes('), enclosed within single quotes." +
        " Single quotes can be omitted if the six reserved characters: (, ), *, + , ?, |, are not used by the terminal</li>" +
        "<li>\"\" denotes the empty string" +
        "<li> &lt;Rightside&gt; is a regular expression over terminals and nonterminals that uses | for disjunction, " +
        "* for closure, white-space for concatenation, parenthesis ( ) for grouping, + for closure without empty string, " +
        " and ? for option (disjunction with empty string)</li></ul>"

    case CYKEx =>
      "TODO"
  }

  def generateRandomWord(g: Grammar) = {
    //select a random length and select a word of that length at random, until a word is found
    var tries = 0
    wordForDerivation = None
    //create operation contexts    
    val gen = new SizeBasedRandomAccessGenerator(g, maxWordLength)(getGlobalContext,
      new EnumerationContext())
    while (!wordForDerivation.isDefined && tries < maxTries) {
      val randLen = minWordLength + (new java.util.Random()).nextInt(maxWordLength - minWordLength)
      val enum = gen.getSamplingEnumerator(g.start, randLen, 1)
      if (enum.hasNext) {
        wordForDerivation = Some(enum.next)
      }
      tries += 1
    }
    wordForDerivation
  }

  def generateProblemStatement(gentry: GrammarEntry, extype: ExType): (Map[String, JsValue], Option[BNFGrammar]) = {

    def pack(intro: String, desc: String) = {
      Map(EXERCISE_DESC.intro -> toJson(intro), EXERCISE_DESC.desc -> toJson(desc))
    }

    extype match {
      case AllGrammarsEx =>
        (pack("Description of the grammar:", gentry.desc), gentry.initGrammar)
      case GrammarEx if gentry.isLL1Entry =>
        (pack("Provide an LL(1) grammar for ", gentry.desc), gentry.initGrammar)
      case GrammarEx =>
        (pack("Provide a grammar for ", gentry.desc), gentry.initGrammar)
      case CNFEx =>
        (pack("Convert the following grammar to Chomsky normal form ",
          gentry.reference.toHTMLString), None)
      case GNFEx =>
        (pack("Convert the following grammar to Griebach normal form ",
          gentry.reference.toHTMLString), None)
      case DerivationEx =>
        //generate a word for derivations              
        generateRandomWord(gentry.refGrammar.fromCNF) match {
          case Some(w) =>
            (pack("Provide a leftmost derivation for the word \"" + wordToString(w) +
              "\" from the grammar ", gentry.reference.toHTMLString), None)
          case _ =>
            (pack("Cannot generate a word for the grammar of size: ", minWordLength.toString), None)
        }
      case CYKEx =>
        val cnfRef = renameAutoSymbols(gentry.cnfRef)
        val wopt = gentry.word match {
          case None =>
            generateRandomWord(gentry.refGrammar.fromCNF)
          case s => s
        }
        val otherfields = Map(CYK_EXERCISES.nonterminals -> toJson(cnfRef.nonTerminals.mkString(",")),
          CYK_EXERCISES.word -> toJson(wopt.map(wordToString).getOrElse("")))
        wopt match {
          case Some(w) => (pack(s"Write the CYK parse table for the word ${wordToString(w)} of the grammar ",
            cnfRef.toHTMLString) ++ otherfields, None)
          case None =>
            (pack("Cannot generate a word for the grammar of size: ", minWordLength.toString) ++ otherfields,
              None)
        }
      case ProgLangEx =>
        val stmt = s"""Refine the grammar for ${gentry.desc} shown in the editor""" +
          " so that it does not accept syntactically incorrect" +
          " programs by eliminating the counter-examples."
        val initGrammar = gentry.initGrammar
        if (!initGrammar.isDefined)
          throw new IllegalStateException("Initial grammar is not defined for problem: " + gentry.id)
        (pack(stmt, ""), initGrammar)
    }
  }

  def getSolution(gentry: GrammarEntry, extype: ExType, userAnswer: String): Option[String] = extype match {
    case GrammarEx | ProgLangEx =>
      Some(gentry.reference.toHTMLString)
    case CNFEx =>
      Some(renameAutoSymbols(gentry.cnfRef).toHTMLString)
    case GNFEx =>
      Some(renameAutoSymbols(GNFConverter.toGNF(gentry.refGrammar)(getGlobalContext))
        .toHTMLString)
    case DerivationEx =>
      Some("Operation not supported!")
    case CYKEx =>
      val rules = userAnswer.split("\n").toList
      val (bnfGrammar, errstr) = (new GrammarParser()).parseGrammar(rules)
      if (!bnfGrammar.isDefined)
        Some("Parse Error:" + errstr)
      else {
        val g = bnfGrammar.get.cfGrammar
        if (wordForDerivation.isDefined) {
          val word = wordForDerivation.get
          val parseWord = word.map(_.asInstanceOf[Terminal])
          val (_, cykTable) = (new CYKParser(g)).parseBottomUpWithTable(parseWord)(getGlobalContext)
          val N = cykTable.length
          var str = ""
          for (k <- 1 to N) // substring length 
            for (p <- 0 to (N - k)) {
              val i = p; val j = p + k - 1;
              str += s"$i-$j:${cykTable(i)(j).toSeq.mkString(",")}\n"
            }
          Some(str)
        } else
          None
      }
  }

  /**
   * Returns a string (which could be a temporary result) and also continuation
   * if more operation has to be performed
   */
  def checkSolution(gentry: GrammarEntry, extype: ExType, userAnswer: String)(implicit gctx: GlobalContext): OpRes = {
    extype match {
      case GrammarEx =>
        //here, we expect the user answer to be a grammar in EBNF form
        val rules = userAnswer.split("\n").toList
        //try to parse the grammar (syntax errors will be displayed in the console)
        val (bnfGrammar, errstr) = (new GrammarParser()).parseGrammar(rules)
        if (!bnfGrammar.isDefined)
          Last(Map(FEEDBACK.text -> s"Parse Error: $errstr"))
        else
          checkGrammarSolution(gentry, bnfGrammar.get)
      case DerivationEx =>
        //here, we expect the userAnswer to be a derivation
        //parse the input string into derivation steps        
        val (derivationSteps, errmsg) = (new SententialFormParser()).parseSententialForms(
          userAnswer.split("\n").toList, gentry.refGrammar)
        if (errmsg.isEmpty()) {
          //get the last word presented
          if (wordForDerivation.isDefined) {
            Last(Map(FEEDBACK.text ->
              checkDerivation(gentry, derivationSteps, wordForDerivation.get)))
          } else
            Last(Map(FEEDBACK.text ->
              "Cannot find the word to derive! Select the problem and try again."))
        } else
          Last(Map(FEEDBACK.text -> s"Parse Error: $errmsg"))
      case CNFEx =>
        //here, we expect the user answer to be a grammar in EBNF form                        
        val (bnfGrammar, errstr) = (new GrammarParser()).parseGrammar(userAnswer.split("\n").toList)
        if (!bnfGrammar.isDefined)
          Last(Map(FEEDBACK.text -> s"Parse Error: $errstr"))
        else
          checkCNFSolution(gentry, bnfGrammar.get)

      case GNFEx =>
        //here, we expect the user answer to be a grammar in EBNF form                        
        val (bnfGrammar, errstr) = (new GrammarParser()).parseGrammar(userAnswer.split("\n").toList)
        if (!bnfGrammar.isDefined)
          Last(Map(FEEDBACK.text -> s"Parse Error: $errstr"))
        else
          checkGNFSolution(gentry, bnfGrammar.get)

      case CYKEx =>
        val userTable = userAnswer.split("\n").map { entry =>
          val Seq(indexPart, nontermPart) = entry.split(":").toSeq
          val tmp1 = indexPart.split("-")
          val tmp2 = tmp1.map(_.toInt)
          val Seq(s, t) = tmp2.toSeq
          val nonterms = nontermPart.split(",").toSet
          ((s, t), nonterms)
        }.toMap
        if (wordForDerivation.isDefined) {
          val cnfg = renameAutoSymbols(gentry.cnfRef) // replying on the determinism of 'renameAutoSymbols' here
          checkCYKSolution(userTable, cnfg, wordForDerivation.get)
        } else
          Last(Map(FEEDBACK.text ->
            "Cannot find the word to parse! Select the problem and try again."))

      case ProgLangEx =>
        //here, we expect the user answer to be a grammar in EBNF form
        val rules = userAnswer.split("\n").toList
        //try to parse the grammar (syntax errors will be displayed in the console)
        val (bnfGrammar, errstr) = (new GrammarParser()).parseGrammar(rules)
        if (!bnfGrammar.isDefined)
          Last(Map(FEEDBACK.text -> s"Parse Error: $errstr"))
        else
          checkProgLangSolution(gentry, bnfGrammar.get)
    }
  }

  import scala.util.control.Breaks._
  def checkCYKSolution(userTable: Map[(Int, Int), Set[String]], cnfg: Grammar, word: Word) = {
    val (_, cykTable) = (new CYKParser(cnfg)).parseBottomUpWithTable(word)(getGlobalContext)
    // compare (valid) entries of the CYK table
    val N = cykTable.length
    var fdb = Map[String, String]()
    breakable {
      for (k <- 1 to N;//) // substring length 
        /*for (*/p <- 0 to (N - k)) {
          val i = p; val j = p + k - 1;
          //if (userTable.contains((i, j))) {
            val ans = cykTable(i)(j).map(_.toString())
            if (userTable.getOrElse((i, j), Set.empty) != ans) {
              fdb = Map(CYK_CHECK.table_feedback -> s"$i-$j: Values for the entry is incorrect",
                FEEDBACK.text -> "The solution has incorrect values for some entries!")
              break
            }
          /*} else {
            fdb = Map(CYK_CHECK.table_feedback -> s"$i-$j: Entry does not exit!",
              FEEDBACK.text -> "The solution is incomplete! there are entries without answers.")
            break
          }*/
        }
    }
    if (fdb.isEmpty)
      Last(Map(FEEDBACK.text -> "Correct!"))
    else
      Last(fdb)
  }

  def checkDerivation(gentry: GrammarEntry, derivationSteps: List[SententialForm], word: Word): String = {
    import DerivationChecker._

    DerivationChecker.checkLeftMostDerivation(word,
      derivationSteps, gentry.refGrammar) match {
        case Correct() =>
          "Correct."
        case InvalidStart() =>
          "Error: Derivation should start with: " + gentry.refGrammar.start
        case InvalidEnd() =>
          "Error: Derivation should end with: " + wordToString(word)
        case WrongStep(from, to, msg) =>
          "Error: cannot derive \"" + wordToString(to) + "\" form \"" + wordToString(from) + "\"" + ": " + msg
        case Other(msg) =>
          "Error: " + msg
      }
  }

  def checkCNFSolution(gentry: GrammarEntry, studentGrammar: BNFGrammar)(implicit opctx: GlobalContext): OpRes = {
    if (BNFConverter.usesRegOp(studentGrammar)) {
      Last(Map(FEEDBACK.text ->
        "The grammar is in EBNF form. You cannot use *,+,? in CNF form"))
    } else {
      val g = studentGrammar.cfGrammar
      CFGrammar.getRuleNotInCNF(g, false) match {
        case None =>
          Partial(Map(FEEDBACK.text ->
            "The grammar staisfies CNF properties.\nchecking for equivalence..."),
            Future {
              checkEquivalence(gentry.cnfRef, g)
            })
        case Some(Error(rule, msg)) =>
          Last(Map(FEEDBACK.text -> s"Rule not in CNF: $rule : $msg"))
      }
    }
  }

  def checkGNFSolution(gentry: GrammarEntry, studentGrammar: BNFGrammar)(implicit opctx: GlobalContext): OpRes = {
    if (BNFConverter.usesRegOp(studentGrammar)) {
      Last(Map(FEEDBACK.text ->
        "The grammar is in EBNF form. You cannot use *,+,? in GNF form"))
    } else {
      val g = studentGrammar.cfGrammar
      CFGrammar.getRuleNotInGNF(g) match {
        case None =>
          Partial(Map(FEEDBACK.text ->
            "The grammar staisfies GNF properties.\nchecking for equivalence..."),
            Future {
              checkEquivalence(gentry.cnfRef, g)
            })
        case Some(Error(rule, msg)) =>
          Last(Map(FEEDBACK.text ->
            s"Rule not in Greibach Normal Form: $rule : $msg"))
      }
    }
  }

  def ll1FeedbackStr(studentGrammar: BNFGrammar): (Boolean, String) = {
    if (BNFConverter.usesRegOp(studentGrammar)) {
      (false, "Regular expression operations *,+,? are not supported by the LL1 check." +
        "Remove them and retry")
    } else {
      GrammarUtils.isLL1WithFeedback(studentGrammar.cfGrammar) match {
        case fb @ GrammarUtils.InLL1() => (true, fb.toString)
        case ll1feedback =>
          (false, ll1feedback.toString)
      }
    }
  }

  def checkLL1(studentGrammar: BNFGrammar): Option[String] = {
    val ll1checkRes = ll1FeedbackStr(studentGrammar)._2
    Some(ll1checkRes + "\n" + (if (this.adminMode) {
      //if in admin mode include first/follow sets
      val (nullables, first, follow) = GrammarUtils.nullableFirstFollow(studentGrammar.cfGrammar)
      "Nullables: " + Util.setString(nullables.toSeq) + "\n" + first.collect {
        case (k: Nonterminal, v) =>
          s"""first($k) -> ${Util.setString(v.toSeq)}"""
      }.mkString("\n") + "\n" + follow.collect {
        case (k: Nonterminal, v) => s"""follow($k) -> ${Util.setString(v.toSeq)}"""
      }.mkString("\n")
    } else
      ""))
  }

  import AmbiguityChecker._

  def checkAmbiguity(studentGrammar: BNFGrammar)(implicit opctx: GlobalContext): String = {
    val g = studentGrammar.cfGrammar

    implicit val ambctx = new AmbiguityContext(maxSize = 50, //note there is not parsing here 
      consecWordsForAmbiguityCheck = 300)
    implicit val ectx = new EnumerationContext()
    val ambChecker = new AmbiguityChecker(g)
    ambChecker.checkAmbiguityInStudentGrammar() match {
      case List() =>
        "The grammar is possibly unambiguous."
      //"The grammar is unambiguous."
      case AmbiguityWitness(ant, w) :: _ =>
        s"Nonterminal $ant is ambiguous " +
          s"- there are at least two parse trees for ${wordToString(w)}"
    }
  }

  def checkGrammarSolution(gentry: GrammarEntry, studentGrammar: BNFGrammar)(implicit opctx: GlobalContext): OpRes = {
    //check if the exercise requires the grammar to be in LL1
    val ll1feedback = if (gentry.isLL1Entry) {
      //check for LL1
      val ll1res = ll1FeedbackStr(studentGrammar)
      if (!ll1res._1) {
        "Warning: LL(1) check failed: " + ll1res._2 + "\n\n"
      } else
        ""
    } else ""
    checkEquivalence(gentry.refGrammar, studentGrammar.cfGrammar) match {
      case Last(resstr) =>
        val fdb = ll1feedback + resstr(FEEDBACK.text)
        Last(Map(FEEDBACK.text -> fdb))
      case Partial(resstr, nextPart) =>
        val fdb = ll1feedback + resstr(FEEDBACK.text)
        Partial(Map(FEEDBACK.text -> fdb), nextPart)
    }
  }

  def checkEquivalence(ref: Grammar, g: Grammar)(implicit opctx: GlobalContext): OpRes = {

    implicit val ectx = new EnumerationContext()
    implicit val eqctx = new EquivalenceCheckingContext(nOfTestcases = 100,
      startSize = 1, maxSize = 11, timeOut = 10 * 1000) //10s
    implicit val verictx = new EquivalenceVerificationContext(verificationTimeout = 20,
      testsForVerification = 100, maxSizeForVerification = 11)
    implicit val pctx = new ParseContext()

    def proveEquivalence(g1: Grammar, g2: Grammar): Boolean = {
      val verifier = new EquivalenceVerifier(g1, g2)
      verifier.proveEquivalence() match {
        case Some(true) => true
        case _ => false
      }
    }

    if (g.cnfGrammar.rules.isEmpty) {
      Last(Map(FEEDBACK.text ->
        "The grammar accepts/produces no strings! Check if all rules are reachable and productive !"))
    } else {

      (new StudentGrammarEquivalenceChecker(ref)).isEquivalentTo(g) match {
        case List() => {
          //print a temporary status message here here, iff this has not been aborted          
          Partial(Map(FEEDBACK.text -> "All testcases passed.\nProving equivalence ... "),
            Future {
              Last(if (proveEquivalence(ref, g))
                Map(FEEDBACK.text -> "Correct.")
              else
                Map(FEEDBACK.text -> "Possibly correct but unable to prove correctness."))
            })
        }
        case NotEquivalentNotAcceptedBySolution(ex) :: _ =>
          Last(Map(FEEDBACK.text ->
            ("The grammar does not accept the string: " + wordToString(ex))))

        case NotEquivalentGeneratedBySolution(ex) :: _ =>
          Last(Map(FEEDBACK.text ->
            ("The grammar generates the invalid string: " + wordToString(ex))))
      }
    }
  }

  /**
   * For now not proving equivalence here
   */
  def checkProgLangSolution(gentry: GrammarEntry, studentGrammar: BNFGrammar)(implicit opctx: GlobalContext): OpRes = {

    val bindir = Play.getFile("/public/resources/").getAbsolutePath() + "/bin"
    implicit val ectx = new EnumerationContext(maxIndexSizeForGeneration = 22) //restrict the index size
    implicit val eqctx = new EquivalenceCheckingContext(nOfTestcases = 100000,
      startSize = 1, maxSize = 15, timeOut = 60 * 1000) //1m
    implicit val pctx = new ParseContext(antlrCompilationDir = bindir,
      antlrJarPath = Play.getFile("/lib/antlr-4.5-complete.jar").getAbsolutePath())

    val ref = gentry.refGrammar
    val g = CFGrammar.appendSuffix("2", studentGrammar.cfGrammar)
    val init = gentry.initGrammar.get.cfGrammar

    if (g.cnfGrammar.rules.isEmpty) {
      Last(Map(FEEDBACK.text -> ("The grammar accepts/produces no strings! " +
        "Check if all rules are reachable and productive !")))
    } else {

      Partial(Map(FEEDBACK.text -> "Checking if your grammar generates invalid strings..."),
        Future {
          val refParser = new AntlrParser(ref)
          (new SamplingBasedEquivalenceChecker(g)).checkPredicateHolds(refParser.parse _) match {
            case _ if (opctx.abort) =>
              refParser.cleanup
              Last(Map(FEEDBACK.text -> "Operation cancelled by user!"))
            case List(ctrex) =>
              refParser.cleanup
              Last(Map(FEEDBACK.text -> ("The grammar generates the invalid string: "
                + wordToString(ctrex))))
            case _ =>
              //check if valid strings of 'initg' is contained in 'g'
              Partial(Map(FEEDBACK.text -> ("Your grammar generates only valid strings!" +
                " Checking if it accepts all valid strings ...")),
                Future {
                  val gParser = if (GNFUtilities.hasIndirectLeftRecursion(g))
                    new CYKParser(g.cnfGrammar)
                  else
                    new AntlrParser(g)

                  def cleanupParsers {
                    //refParser.cleanup
                    gParser match {
                      case antlrp: AntlrParser =>
                        antlrp.cleanup
                      case _ => ;
                    }
                  }
                  //parsed by 'ref' but not by 'g' ?
                  val res = (new SamplingBasedEquivalenceChecker(init)).checkPredicateHolds(
                    x => !refParser.parse(x) || gParser.parse(x))
                  cleanupParsers
                  res match {
                    case _ if (opctx.abort) =>
                      cleanupParsers
                      Last(Map(FEEDBACK.text -> "Operation cancelled by user!"))
                    case List(ex) =>
                      Last(Map(FEEDBACK.text ->
                        ("The grammar does not accept the string: " + wordToString(ex))))
                    case _ =>
                      Last(Map(FEEDBACK.text ->
                        "Hurray! No counter-examples found!"))
                  }
                })
          }
        })
    }
  }

  def provideHints(ex: GrammarEntry, studentGrammar: BNFGrammar)(implicit opctx: GlobalContext): OpRes = {

    implicit val ectx = new EnumerationContext()
    implicit val eqctx = new EquivalenceCheckingContext(nOfTestcases = 100,
      startSize = 1, maxSize = 11, timeOut = 10 * 1000) //10s
    implicit val repairctx = new RepairContext(enableExpensiveRepair = true,
      nCorrectWordsForRepair = 100)
    implicit val pctx = new ParseContext()
    val maxHintsSize = 5

    def prettyPrintFeedbacks(inG: Grammar, resG: Grammar, w: Word, feedbacks: List[GrammarFeedback]) = {
      //pretty print feedbacks by renaming new non-terminals to simpler names
      val newnonterms = resG.nonTerminals.filterNot(CNFConverter.isCNFNonterminal _).toSet --
        nonterminals(inG)
      val renameMap = genRenameMap(newnonterms.toList, nonterminals(resG))
      feedbacks.map(f => {
        val feedbackString = f match {
          case AddAllRules(rules) =>
            var msg = "To accept the string: " + wordToString(w) + "\n"
            val newMap = renameMap.map {
              //add question mark to the names of the nonterminals
              case (k, Nonterminal(n)) => (k, Nonterminal(n + "?"))
            }
            msg += "Try adding: " + rulesToStr(replace(rules, newMap))
            if (!newnonterms.isEmpty) {
              //println("newnontemrs: " + newnonterms)
              msg + "\nNonterminals with '?' could belong to the grammar or could be a fresh nonterminal"
            } else
              msg

          case RemoveRules(rules, None) =>
            "To block the string: " + wordToString(w) +
              "\nRemove: " + rulesToStr(rules)

          case RemoveRules(rules, Some(repairRules)) =>
            "To block the string: \"" + wordToString(w) + "\"" +
              "\nRemove the rule: " + rulesToStr(rules) +
              " that results in the derivation of the invalid string"

          case RefineRules(olds, news, repairRules) =>
            "To block the string: \"" + wordToString(w) + "\"" +
              " the derivation: " + rulesToStr(repairRules) +
              " should be blocked in context: " + rulesToStr(olds) +
              "\nTry replacing: " + rulesToStr(olds) +
              "\nby: " + rulesToStr(replace(news, renameMap))

          case ExpandRules(olds, news) =>
            "Grammar generates invalid string: \"" + wordToString(w) + "\"" +
              "\nTry hints after expanding the righside of the rule: " + rulesToStr(olds) +
              "\nby inlining the productions of the nonterminals in the rightside: " +
              "\nEg. as " + (if (news.size <= maxHintsSize)
                rulesToStr(replace(news, renameMap))
              else
                rulesToStr(replace(news.take(5), renameMap)) + " ...")

          case NoRepair(reason) =>
            "Cannot provide hints: " + reason
        }
        feedbackString
      }).mkString("\n")
    }

    val resstr = if (BNFConverter.usesRegOp(studentGrammar)) {
      "The grammar is in EBNF form. Click on \"Normalize\" and retry."
    } else {
      val plainGrammar = ebnfToGrammar(studentGrammar)
      if (!CFGrammar.isNormalized(plainGrammar)) {
        "Some rules are not normalized. Click on \"Normalize\" and retry."
      } else {

        val cnfG = CNFConverter.toCNF(plainGrammar)
        if (cnfG.rules.isEmpty) {
          "The grammar is empty. Not all rules are produtive and reachable."
        } else {
          val equivChecker = new StudentGrammarEquivalenceChecker(ex.cnfRef)
          val repairer = new Repairer(equivChecker)
          equivChecker.isEquivalentTo(cnfG) match {
            case List() =>
              "The grammar is probably correct. Try checking this grammar."
            case (res @ NotEquivalentNotAcceptedBySolution(ex)) :: _ =>
              val (resG, feedbacks) = repairer.hint(cnfG, res)
              prettyPrintFeedbacks(plainGrammar, resG, ex, feedbacks)

            case (res @ NotEquivalentGeneratedBySolution(ex)) :: _ =>
              val (resG, feedbacks) = repairer.hint(cnfG, res)
              prettyPrintFeedbacks(plainGrammar, resG, ex, feedbacks)
          }
        }
      }
    }
    Last(Map(FEEDBACK.text -> resstr))
  }
}

