case class Word(s: String)
case class Sentence(ws: List[Word])

object TextStatistics {

  val s =
    "lorem ipsum dolor sit amet consectetur lorem ipsum et mihi quoniam et adipiscing elit.sed quoniam et advesperascit et mihi ad villam revertendum est nunc quidem hactenus ex rebus enim timiditas non ex vocabulis nascitur.nummus in croesi divitiis obscuratur pars est tamen divitiarum.nam quibus rebus efficiuntur voluptates eae non sunt in potestate sapientis.hoc mihi cum tuo fratre convenit.qui ita affectus beatum esse numquam probabis duo reges constructio interrete.de hominibus dici non necesse est.eam si varietatem diceres intellegerem ut etiam non dicente te intellego parvi enim primo ortu sic iacent tamquam omnino sine animo sint.ea possunt paria non esse.quamquam tu hanc copiosiorem etiam soles dicere.de quibus cupio scire quid sentias.universa enim illorum ratione cum tota vestra confligendum puto.ut nemo dubitet eorum omnia officia quo spectare quid sequi quid fugere debeant nunc vero a primo quidem mirabiliter occulta natura est nec perspici nec cognosci potest.videmusne ut pueri ne verberibus quidem a contemplandis rebus perquirendisque deterreantur sunt enim prima elementa naturae quibus auctis virtutis quasi germen efficitur.nam ut sint illa vendibiliora haec uberiora certe sunt.cur deinde metrodori liberos commendas.mihi inquam qui te id ipsum rogavi nam adhuc meo fortasse vitio quid ego quaeram non perspicis.quibus ego vehementer assentior.cur iustitia laudatur mihi enim satis est ipsis non satis.quid est enim aliud esse versutum nobis heracleotes ille dionysius flagitiose descivisse videtur a stoicis propter oculorum dolorem.diodorus eius auditor adiungit ad honestatem vacuitatem doloris.nos quidem virtutes sic natae sumus ut tibi serviremus aliud negotii nihil habemus."

  def sentencesFromString: String => List[Sentence] = { s =>
    s.split('.')
      .toList
      .filterNot(_.isEmpty)
      .map(s1 => Sentence(s1.split(' ').toList.filterNot(_.isEmpty).map(Word)))
  }

  def wordCount: List[Sentence] => Int = s => s.map(wordCountSentence).sum

  def wordCountSentence: Sentence => Int = { case Sentence(ws) =>
    ws.length
  }

  def sentenceCount: List[Sentence] => Int = _.length

  def longestWord: List[Sentence] => Int = _.map(longestWordInSentence).max

  def longestWordInSentence: Sentence => Int = { case Sentence(ws) =>
    ws.map { case Word(s) =>
      s.length
    }.max
  }

  def sixMostFrequentWords: List[Sentence] => List[Word] = ???

}
