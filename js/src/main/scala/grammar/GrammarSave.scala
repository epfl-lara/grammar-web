package grammar

/**
 * Created by Mikael on 13.08.2015.
 * Will save the current grammar to the given field
 */
object GrammarSave {
  object Reference extends GrammarSave
  object Initial extends GrammarSave
  object None extends GrammarSave
}
sealed trait GrammarSave
