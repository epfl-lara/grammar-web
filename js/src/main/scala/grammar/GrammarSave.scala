package grammar

/**
 * Created by Mikael on 13.08.2015.
 * Will save the current grammar to the given field
 */
object GrammarSave {
  case object Reference extends GrammarSave
  case object Initial extends GrammarSave
  case object None extends GrammarSave
}
sealed trait GrammarSave
