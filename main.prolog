:- use_module(library(date_time)).

:- discontiguous verb/1.
:- discontiguous conj/1.
:- discontiguous phrase/1.
:- discontiguous etymology/2.
:- discontiguous meaning/2.
:- discontiguous example/2.
:- discontiguous comes_from/2.
:- discontiguous similar/2.
:- discontiguous word_topic/2.

entry_type(conj).
entry_type(phrase).
entry_type(verb).
entry_type(adv).
entry_type(adj).
entry_type(pron).
entry_type(noun).
entry_type(adposition).
entry_type(interjection).

word(W) :- verb(W).
word(W) :- adj(W).
word(W) :- adv(W).
word(W) :- pron(W).
word(W) :- noun(W).

verb(V) :- meaning(verb(V), _).
adj(A) :- meaning(adj(A), _).
adv(A) :- meaning(adv(A), _).
pron(A) :- meaning(pron(A), _).
noun(A) :- meaning(noun(A), _).
conj(C) :- meaning(conj(C), _).
phrase(C) :- meaning(phrase(C), _).
adposition(P) :- meaning(adposition(P), _).
interjection(I) :- meaning(interjection(I), _).

meaning(verb(antaa), '[annan] to give').

meaning(adj(eri), 'different').
similar(adj(eri), adj(erilainen)).

meaning(adv(heti), immediately).

meaning(adv(ensin), 'at first').

meaning(pron(jossa), 'where, in which').
example(pron(jossa), 'Haluaisin huoneen, jossa on ilmastointi.').

meaning(adv(joten), 'therefore').
example(adv(joten), 'Oli lämmin, joten avasin ikkunan.').

meaning(adv(kuitenkin), 'however').

meaning(adv(paitsi), 'except').
example(adv(paitsi), 'Kaikki paitsi Jim tulivat.').

meaning(conj(vaikka), 'even if').
example(conj(vaikka), 'En voi vaikka haluan.').

meaning(conj(kunnes), 'until').
example(conj(kunnes), 'Odota kunnes tulen takaisin.').

meaning(conj('sekä - että'), 'both').
similar(conj('sekä - että'), pron(molemmat)).
example(conj('sekä - että'), 'Olet sekä nätti että ystävällinen.').

meaning(adj(nätti), 'pretty').
example(adj(nätti), 'Olet sekä nätti että ystävällinen.').

meaning(conj(vaan), 'but').
example(conj(vaan), 'Se ei ole musta, vaan punainen.').
similar(conj(vaan), conj(vain)).

meaning(conj(sillä), 'since (because)').

meaning(verb(esiintyä), '[esiinnyn] to occur').

meaning(noun(yhteys_1), 'connection').
meaning(noun(yhteys_2), 'context').

meaning(noun(perustelu), 'reasoning').

meaning(verb(perustella), '[perustelen] to argue, justify, explain').
comes_from(noun(perustelu), verb(perustella)).

meaning(phrase('jotain lisättävää'), 'something to add (to a conversation)').

meaning(noun(nuha), 'runny nose').
example(noun(nuha), 'Minulla on nuha ja olen nukkunut huonosti.').

meaning(noun(päiväunet), 'nap').
example(noun(päiväunet), 'Olen sekä nukkunut päiväunet että juonut kuumaa mehua.').

meaning(conj('joko - tai'), 'either - or').
example(conj('joko - tai'), 'Me ostamme joko kissan tai koiran.').

meaning(verb(levätä), '[lepään] to rest').
example(verb(levätä), 'Lepään kotona').

meaning(verb(jäädä), '[jään] to stay').
example(verb(jäädä), 'En mene tänään töihin vaan jään kotiin').

meaning(adv(innoissaan), 'excitedly').
example(adv(innoissaan), '"Kivaa", hän sanoi innoissaan.').
comes_from(adv(innoissaan), verb(innostaa)).

meaning(verb(innostaa), '[innostan] to excite, inspire').
example(verb(innostaa), 'Mikä innostaa sinua?').

meaning(phrase('sitä paitsi'), 'besides, in addition to (that)').
example(phrase('sitä paitsi'), 'Sitä paitsi olen vanhempi kuin sinä.').

meaning(adj(liittyvä), 'related').
example(adj(liittyvä), 'liittyvät tehtävät').

comes_from(adj(liittyvä), verb(liittyä)).

meaning(verb(liittyä), '[liityn] to join (a group), to be related to, to be part of').

meaning(verb(muokata), '[muokkaan] to edit/modify').

meaning(adj(yleinen), 'general').

meaning(adv(yleensä), 'usually, generally').
example(adv(yleensä), 'He menevät yleensä ulos lauantaisin.').

meaning(noun(verkkosivusto), 'website').
etymology(noun(verkkosivusto), 'verkko (net) + sivu (page) + -sto (place)').

meaning(noun(viesti), 'message').

meaning(verb(kumota), '[kumoan] to undo, cancel').

meaning(noun(tallennustila), 'storage space').

meaning(noun(tausta), 'background').

meaning(noun(ohjaus), 'control').
comes_from(noun(ohjaus), verb(ohjata)).

meaning(verb(ohjata), '[ohjaan] to control').

meaning(noun(kirkkaus), 'brightness').
comes_from(noun(kirkkaus), adj(kirkas)).

meaning(adj(kirkas), 'bright').

meaning(noun(taustakuva), 'wallpaper').

meaning(noun(nauha), 'tape, ribbon, band, string').

meaning(noun(mittanauha), 'measuring tape').

meaning(verb(ladata), '[lataan] to download').

meaning(verb(tyhjentää), '[tyhjennän] to empty, to reset').

meaning(conj(mikäli), 'if, provided that, in the event that').
example(conj(mikäli), 'Mikäli tämän ajan kuluessa varausta ei makseta, varauksenne peruuntuu.').

meaning(noun(vahinko), 'mishap, accident, damage').

meaning(adv(liikaa), 'too much').
example(adv(liikaa), 'Elämässä ei ole koskaan liikaa laulua ja hyvää seuraa.').

meaning(verb(kutsua), '[kutsun] to invite').

meaning(noun(tiski), 'counter (bar)').
example(noun(tiski), 'tehdä tilauksesi tiskillä').

meaning(adv(etukäteen), 'in advance').
etymology(adv(etukäteen), 'etu- (“fore-”) +‎ käteen (“hand”)').

meaning(adposition(kuluessa), 'within').
example(adposition(kuluessa), 'He soittavat meille viikon kuluessa.').

meaning(interjection('kissan viikset'), 'nonsense, rubbish').

meaning(noun(viikset), 'mustache').

meaning(verb(korostaa), '[korostan] emphasize, highlight').

meaning(conj(jotta), 'so that').
example(conj(jotta), 'Nousimme aikaisin, jotta näkisimme auringonnousun.').

meaning(noun(ikä), 'age, lifetime').

meaning(noun(käyttöikä), '(battery\'s lifespan').
comes_from(noun(käyttöikä), noun(ikä)).

meaning(noun(valinta), 'selection, choice').

meaning(adj(virallinen), 'official').

meaning(noun(ominaisuus), 'property, quality, feature').
comes_from(noun(ominaisuus), adj(ominainen)).

meaning(adj(ominainen), 'typical, characteristic').
example(adj(ominainen), 'Tämä on Japanille ominainen tapa.').

meaning(noun(koru), 'jewel').

meaning(verb(omistaa), '[omistan] to own').
example(verb(omistaa), 'Minä omistan talon ja maatilan.').

meaning(noun(maatila), 'farm').

meaning(adj(eronnut), 'divorced').

meaning(verb(viettää), '[vietän] to spend time, to pass the time').

meaning(adj(naimisissa), 'married').

meaning(noun(avomies), 'husband').
meaning(noun(avovaimo), 'wife').
meaning(noun(puoliso), 'spouse').

meaning(noun(lakimies), 'lawyer').

meaning(verb(kantaa), '[kannan] to carry').
example(verb(kantaa), 'Tom kantaa sinua.').

meaning(noun(asukas), 'resident').

meaning(verb(haitata), '[haittaan] to bother, to hinder').
example(verb(haitata), 'Kylmä ei haittaa ketään').

meaning(adv(nykyään), 'recently, nowadays').

meaning(adj(tavallinen), 'usual, ordinary, common, trivial, standard').

meaning(phrase('omin voimin'), 'on your/their own').

meaning(adj(tälläinen), 'such, this kind of').

meaning(noun(some), 'social media').

meaning(verb(levitä), '[leviän] to spread (words)').
example(verb(levitä), 'sana leviää nopeasti ja helposti').

meaning(noun(kustannus), 'cost, expense').

meaning(noun(vapaaehtoinen), 'a volunteer').
meaning(adj(vapaaehtoinen), 'voluntary').

meaning(verb(osallistua), '[osallistun] to participate, to attend').

meaning(adv(mielellään), 'gladly, willingly').

meaning(noun(talkootyö), 'voluteer work').

meaning(noun(astia), 'dish, plate').

meaning(adj(ylimääräinen), 'excessive, redundant').

meaning(noun(tavara), 'goods, wares, belongings').

meaning(noun(puuhamies), 'organizer, especially one who works on a voluntary basis.').

meaning(noun(menestys), 'success').
meaning(noun(menetys), 'loss').

meaning(verb(jännittää), '[jännitän] to tense up, to feel excited and nervous').
example(verb(jännittää), 'Mä muistan hyvin, miten mua jännitti.').

meaning(adj(kamala), 'horrible').
meaning(noun(pakkanen), 'frost').

meaning(adj(tyytyväinen), 'content, happy').

meaning(noun(haastattelu), 'an interview').

meaning(pron(muutama), 'a few, some').
example(pron(muutama), 'Oscar myy kolme kirja muutamalla eurolla').

meaning(verb(jutella), '[juttelen] to chat').
example(verb(jutella), 'asiakas juttele hetken').

meaning(noun(takki), 'jacket, coat').
meaning(noun(paita), 'shirt').

meaning(adj(satunnainen), 'random').
meaning(noun(ohikulkija), 'passer-by').

meaning(noun(väline), 'tool, equipment').
meaning(noun(urheiluväline), 'sports equipment').

meaning(verb(laittaa), '[laitan] put, set, place').
example(verb(laittaa), 'Laita sanat oikeaan laatikkoon.').

meaning(noun(saatavuus), 'availability').

meaning(noun(veloitus), 'fee, charge (amount of money levied for a service)').

meaning(noun(valokuitu), 'optical fibre').

meaning(adj(uudenveroinen), 'like new').

% https://www.speaklanguages.com/finnish/vocab/the-human-body
meaning(noun(kulmakarva), 'eyebrow').
word_topic(noun(kulmakarva), topic('human body part')).

meaning(noun(nenä), 'nose').
word_topic(noun(nenä), topic('human body part')).

meaning(noun(suu), 'mouth').
word_topic(noun(suu), topic('human body part')).

meaning(noun(hammas), 'tooth').
word_topic(noun(hammas), topic('human body part')).

meaning(noun(huuli), 'lip').
word_topic(noun(huuli), topic('human body part')).

meaning(noun(parta), 'beard').
word_topic(noun(parta), topic('human body part')).

meaning(noun(niska), 'neck').
word_topic(noun(niska), topic('human body part')).

meaning(noun(olkapää), 'shoulder').
word_topic(noun(olkapää), topic('human body part')).

meaning(noun(kyynärpää), 'elbow').
word_topic(noun(kyynärpää), topic('human body part')).

meaning(noun(ranne), 'wrist').
word_topic(noun(ranne), topic('human body part')).

meaning(noun(rinta), 'chest').
word_topic(noun(rinta), topic('human body part')).

meaning(noun(polvi), 'knee').
word_topic(noun(polvi), topic('human body part')).

meaning(noun(varvas), 'toe').
word_topic(noun(varvas), topic('human body part')).

meaning(noun(mahtavuus), 'mightiness, power, grandiosity').

meaning(adv(riittävästi), 'sufficiently').

meaning(phrase('mahdollisimman pian'), 'as soon as possible').

meaning(phrase('hallitus koolla'), 'the government gathered').

meaning(verb(aiheuttaa), '[aiheutan] to cause, to lead to, to result in').
example(verb(aiheuttaa), 'aiheuttaa hälytyksen').

meaning(adj(ajankohtainen), 'current, of interest (situation)').

meaning(adv(ajallaan), 'on time').

meaning(noun(sielu), 'soul').

meaning(noun(tyyny), 'pillow, cushion').

meaning(noun(jakkara), 'stool').

meaning(noun(valikko), 'menu').

meaning(noun(valintaikkuna), 'dialog box').

meaning(noun(valuutta), 'currency (money)').

meaning(noun(laajennus), 'extension, add-on').

meaning(noun(yksityisyys), 'privacy').

meaning(verb(jakaa), '[jaan] to share, to divide').

meaning(noun(joukkoliikenne), 'mass transportation').

meaning(verb(arpoa), '[arvon] to draw/cast lots (bốc thăm xổ số)').

meaning(noun(oikotie), 'shortcut').

meaning(verb(luoda), '[luon] to create').
example(verb(luoda), 'Alussa Jumala loi taivaan ja maan.').

meaning(verb(hylätä), '[hylkään] to refuse, to reject (an invitation)').

meaning(noun(päätös), 'decision, resolution').
comes_from(noun(päätös), verb(päättää)).

meaning(verb(päättää), '[päätän] to decide/to choose (to do something), to judge').

meaning(noun('tammikuu'), 'month 1').
meaning(noun('helmikuu'), 'month 2').
meaning(noun('maaliskuu'), 'month 3').
meaning(noun('huhtikuu'), 'month 4').
meaning(noun('toukokuu'), 'month 5').
meaning(noun('kesäkuu'), 'month 6').
meaning(noun('heinäkuu'), 'month 7').
meaning(noun('elokuu'), 'month 8').
meaning(noun('syyskuu'), 'month 9').
meaning(noun('lokakuu'), 'month 10').
meaning(noun('marraskuu'), 'month 11').
meaning(noun('joulukuu'), 'month 12').

meaning(noun(harrastus), 'hobby').

meaning(noun(kokoaminen), 'assembly').

meaning(noun(torkku), 'nap, snooze').

meaning(verb(kellua), '[kellun] (intransitive) to float').

meaning(adj(linjoilla), 'online').

meaning(noun(voimassaolo), 'validity').
etymology(noun(voimassaolo), 'voimassa (“in effect, effective”) +‎ olo (“being; the state of”)').

meaning(conj(eli), 'a.k.a, in other words').

meaning(adv(jopa), 'up to, as much as').

meaning(adv(taas), 'again').

meaning(verb(tutustua), '[tutustun] to check smth out, to get acquainted with').

meaning(adv('mm.'), '[muun muassa] among other things').

meaning(verb(päästä), '[pääsen] to get to, reach, arrive').
example(verb(päästä), 'Kuinka pääsen lentokentälle?').

meaning(noun('välilehti'), '(browser\'s) tab').

meaning(noun(vihollinen), 'an enemy').

meaning(verb(joutua), '[joudun + -iin] to involutarily end up in a negative place/situation').

meaning(pron(jokin), 'something').

meaning(verb(todeta), '[totean] to notice, to find out something').

meaning(adposition(aikana), '[genitive + aikana] within, in smth\'s time').
example(adposition(aikana), 'kahden vuoden aikana').

meaning(adj(tärkeä), 'important').

meaning(adv('saman tien'), 'on the spot, immediately').

meaning(verb(vaatia), '[vaadin] to demand, require').

meaning(verb(esittää), '[esitän] to show, to express, to perform, to portray').

meaning(conj('ts.'), '(toisin sanoen) in other words').

meaning(noun(kisa), 'competition, game').

meaning(adj(entinen), 'former (soviet union)').

meaning(verb(aloittaa), '[aloitan] to start, to begin').

meaning(noun(syy), 'reason').

meaning(verb(kasvaa), '[kasvan] to grow').

meaning(noun(kilpailu), 'contest, race; rivalry').

meaning(noun(joukkue), 'a team (sport)').

meaning(noun(valtio), 'government').

meaning(verb(arvioida), '[arvioin] to estimate, to grade').

meaning(adv(edelleen), 'still').

meaning(noun(joukko), 'group, band').

meaning(adj(kansainvälinen), 'international').

meaning(verb(tuntea), '[tunnen] to feel, to sense').

meaning(adv(miten), 'how').

meaning(noun(tavoite), 'target, goal, objective, aim').

meaning(noun(voitto), 'victory').

meaning(noun(ottelu), '(sports) match').

meaning(verb(lisätä), '[lisään] to add').

meaning(verb(jättää), '[jätän] to leave').

meaning(noun(yhteistyö), 'cooperation').

meaning(noun(tutkimus), 'research').

meaning(verb(yrittää), '[yritän] to try').

meaning(noun(auringonlasku), 'sunset').
meaning(noun(auringonnousu), 'sunrise').

meaning(noun(neuvo), 'advice').

meaning(noun(tulkkaus), 'interpretation').

meaning(verb(pyytää), '[pyydän] to request').

meaning(noun(pyyntö), 'a request').
comes_from(noun(pyyntö), verb(pyytää)).

meaning(noun(viranomainen), 'an official, public officer').

meaning(verb(hankkia), '[hankin] to obtain, get, find; purchase').

meaning(adv(muutenkin), 'anyway').

meaning(adj(vakinainen), 'permanent').

meaning(noun(pakolainen), 'refugee').

meaning(noun(velvollisuus), 'responsibility, duty, obligation').

meaning(verb(aikoa), '[aion + inf] intend to, plan to').

meaning(noun(oleskelulupa), 'residence permit').

meaning(noun(työsuhde), 'employment').

meaning(adv(todella), 'really').

meaning(noun(yhteiskunta), 'society').

meaning(verb(käynnistää), '[käynnistän] launch, start').

meaning(verb(estää), '[estän] to prevent, to stop, to hinder').

meaning(noun(nauhoitus), 'recording').
comes_from(noun(nauhoitus), verb(nauhoittaa)).

meaning(verb(nauhoittaa), '[nauhoitan] to tape, record').

meaning(noun(ohjauslevy), 'touchpad').

meaning(noun(jako), '(media) sharing').

meaning(verb(koskea), '[kosken] to touch; to apply to, to concern').

meaning(noun(toteuttaminen), 'realization, execution').
comes_from(noun(toteuttaminen), verb(toteuttaa)).

meaning(verb(toteuttaa), '[toteutan] to realise/realize, carry out, put into practice, implement').

meaning(noun(hengitys), 'breathing').
comes_from(noun(hengitys), verb(hengittää)).

meaning(verb(hengittää), '[hengitän] to breathe').
comes_from(verb(hengittää), noun(henki)).

meaning(noun(henki), 'breath').

meaning(noun(sateenkaari), 'rainbow').

meaning(noun(eduskunta), 'Finnish Parliament').

meaning(noun(määrä), 'amount, quantity; target, aim, ambition').

meaning(noun(määräys), 'order, command, instruction').
comes_from(noun(määräys), verb(määrätä)).

meaning(verb(määrätä), '[määrään] to order, command, instruct').

%    5 - perfect response
%    4 - correct response after a hesitation
%    3 - correct response recalled with serious difficulty
%    2 - incorrect response; where the correct one seemed easy to recall
%    1 - incorrect response; the correct one remembered
%    0 - complete blackout.
memo_score(verb(antaa), date(2020, 2, 20), 5).
memo_score(verb(esiintyä), date(2020, 2, 20), 0).
memo_score(verb(perustella), date(2020, 2, 20), 3).
memo_score(verb(levätä), date(2020, 2, 20), 0).
memo_score(verb(jäädä), date(2020, 2, 20), 4).
memo_score(verb(innostaa), date(2020, 2, 20), 2).
memo_score(verb(liittyä), date(2020, 2, 20), 2).
memo_score(adv(heti), date(2020, 2, 20), 5).
memo_score(adv(ensin), date(2020, 2, 20), 5).
memo_score(adv(joten), date(2020, 2, 20), 2).
memo_score(adv(kuitenkin), date(2020, 2, 20), 5).
memo_score(adv(paitsi), date(2020, 2, 20), 4).
memo_score(adv(innoissaan), date(2020, 2, 20), 4).
memo_score(adj(eri), date(2020, 2, 20), 4).
memo_score(adj(nätti), date(2020, 2, 20), 4).
memo_score(adj(liittyvä), date(2020, 2, 20), 4).
memo_score(pron(jossa), date(2020, 2, 20), 4).
memo_score(noun(yhteys_1), date(2020, 2, 20), 4).
memo_score(noun(yhteys_2), date(2020, 2, 20), 4).
memo_score(noun(perustelu), date(2020, 2, 20), 4).
memo_score(noun(nuha), date(2020, 2, 20), 3).
memo_score(noun(päiväunet), date(2020, 2, 20), 3).
memo_score(conj(vaikka), date(2020, 2, 20), 0).
memo_score(conj(kunnes), date(2020, 2, 20), 4).
memo_score(conj('sekä - että'), date(2020, 2, 20), 0).
memo_score(conj('vaan'), date(2020, 2, 20), 3).
memo_score(conj(sillä), date(2020, 2, 20), 0).
memo_score(conj('joko - tai'), date(2020, 2, 20), 3).
memo_score(phrase('jotain lisättävää'), date(2020, 2, 20), 3).
memo_score(phrase('sitä paitsi'), date(2020, 2, 20), 0).

memo_score(conj(vaikka), date(2020, 2, 20), 1).
memo_score(conj('sekä - että'), date(2020, 2, 20), 4).
memo_score(conj('vaan'), date(2020, 2, 20), 4).
memo_score(conj('sillä'), date(2020, 2, 20), 1).
memo_score(phrase('jotain lisättävää'), date(2020, 2, 20), 4).
memo_score(phrase('sitä paitsi'), date(2020, 2, 20), 4).
memo_score(verb('esiintyä'), date(2020, 2, 20), 0).
memo_score(verb('perustella'), date(2020, 2, 20), 4).
memo_score(verb('levätä'), date(2020, 2, 20), 0).
memo_score(verb('innostaa'), date(2020, 2, 20), 4).
memo_score(verb('innostaa'), date(2020, 2, 20), 4).
memo_score(verb('liittyä'), date(2020, 2, 20), 4).
memo_score(adv('liittyä'), date(2020, 2, 20), 4).
memo_score(adv('joten'), date(2020, 2, 20), 4).
memo_score(noun('nuha'), date(2020, 2, 20), 4).
memo_score(noun('päiväunet'), date(2020, 2, 20), 4).

memo_score(conj(vaikka), date(2020, 2, 20), 4).
memo_score(conj(sillä), date(2020, 2, 20), 4).

memo_score(conj('joko - tai'), date(2020, 2, 21), 5).
memo_score(conj('mikäli'), date(2020, 2, 21), 4).
memo_score(verb('muokata'), date(2020, 2, 21), 4).
memo_score(verb('kumota'), date(2020, 2, 21), 4).
memo_score(verb('ohjata'), date(2020, 2, 21), 3).
memo_score(verb('ladata'), date(2020, 2, 21), 5).
memo_score(verb('tyhjentää'), date(2020, 2, 21), 4).
memo_score(verb('kutsua'), date(2020, 2, 21), 4).
memo_score(verb('korostaa'), date(2020, 2, 21), 4).
memo_score(adv('yleensä'), date(2020, 2, 21), 4).
memo_score(adv('liikaa'), date(2020, 2, 21), 3).
memo_score(adv('etukäteen'), date(2020, 2, 21), 4).
memo_score(adj('yleinen'), date(2020, 2, 21), 5).
memo_score(adj('kirkas'), date(2020, 2, 21), 4).
memo_score(noun('verkkosivusto'), date(2020, 2, 21), 5).
memo_score(noun('viesti'), date(2020, 2, 21), 5).
memo_score(noun('viesti'), date(2020, 2, 21), 5).
memo_score(noun('tallennustila'), date(2020, 2, 21), 4).
memo_score(noun('tausta'), date(2020, 2, 21), 3).
memo_score(noun('ohjaus'), date(2020, 2, 21), 4).
memo_score(noun('kirkkaus'), date(2020, 2, 21), 4).
memo_score(noun('taustakuva'), date(2020, 2, 21), 4).
memo_score(noun('nauha'), date(2020, 2, 21), 0).
memo_score(noun('mittanauha'), date(2020, 2, 21), 4).
memo_score(noun('vahinko'), date(2020, 2, 21), 4).
memo_score(noun('tiski'), date(2020, 2, 21), 4).
memo_score(noun('viikset'), date(2020, 2, 21), 0).
memo_score(adposition('kuluessa'), date(2020, 2, 21), 3).
memo_score(interjection('kissan viikset'), date(2020, 2, 21), 4).

memo_score(verb('ohjata'), date(2020, 2, 21), 5).
memo_score(adv('liikaa'), date(2020, 2, 21), 4).
memo_score(noun('tausta'), date(2020, 2, 21), 4).
memo_score(noun('nauha'), date(2020, 2, 21), 4).
memo_score(noun('viikset'), date(2020, 2, 21), 4).
memo_score(adposition('kuluessa'), date(2020, 2, 21), 4).

memo_score(conj('jotta'), date(2020, 2, 22), 4).
memo_score(adj('virallinen'), date(2020, 2, 22), 5).
memo_score(adj('ominainen'), date(2020, 2, 22), 2).
memo_score(noun('ikä'), date(2020, 2, 22), 4).
memo_score(noun('käyttöikä'), date(2020, 2, 22), 4).
memo_score(noun('valinta'), date(2020, 2, 22), 5).
memo_score(noun('ominaisuus'), date(2020, 2, 22), 4).
memo_score(noun('koru'), date(2020, 2, 22), 4).
memo_score(adj('ominainen'), date(2020, 2, 22), 4).

memo_score(phrase('omin voimin'), date(2020, 2, 22), 4).
memo_score(verb('omistaa'), date(2020, 2, 22), 0).
memo_score(verb('viettää'), date(2020, 2, 22), 4).
memo_score(verb('kantaa'), date(2020, 2, 22), 1).
memo_score(verb('haitata'), date(2020, 2, 22), 4).
memo_score(verb('levitä'), date(2020, 2, 22), 0).
memo_score(verb('osallistua'), date(2020, 2, 22), 1).
memo_score(verb('jännittää'), date(2020, 2, 22), 3).
memo_score(verb('jutella'), date(2020, 2, 22), 4).
memo_score(verb('laittaa'), date(2020, 2, 22), 4).
memo_score(adv('nykyään'), date(2020, 2, 22), 4).
memo_score(adv('mielellään'), date(2020, 2, 22), 4).
memo_score(adj('eronnut'), date(2020, 2, 22), 5).
memo_score(adj('naimisissa'), date(2020, 2, 22), 5).
memo_score(adj('tavallinen'), date(2020, 2, 22), 2).
memo_score(adj('tälläinen'), date(2020, 2, 22), 5).
memo_score(adj('vapaaehtoinen'), date(2020, 2, 22), 1).
memo_score(adj('ylimääräinen'), date(2020, 2, 22), 4).
memo_score(adj('kamala'), date(2020, 2, 22), 4).
memo_score(adj('tyytyväinen'), date(2020, 2, 22), 4).
memo_score(adj('satunnainen'), date(2020, 2, 22), 4).
memo_score(pron('muutama'), date(2020, 2, 22), 4).
memo_score(noun('maatila'), date(2020, 2, 22), 4).
memo_score(noun('avomies'), date(2020, 2, 22), 4).
memo_score(noun('avovaimo'), date(2020, 2, 22), 4).
memo_score(noun('puoliso'), date(2020, 2, 22), 4).
memo_score(noun('lakimies'), date(2020, 2, 22), 0).
memo_score(noun('asukas'), date(2020, 2, 22), 5).
memo_score(noun('some'), date(2020, 2, 22), 4).
memo_score(noun('kustannus'), date(2020, 2, 22), 4).
memo_score(noun('vapaaehtoinen'), date(2020, 2, 22), 4).
memo_score(noun('talkootyö'), date(2020, 2, 22), 4).
memo_score(noun('astia'), date(2020, 2, 22), 5).
memo_score(noun('tavara'), date(2020, 2, 22), 4).
memo_score(noun('puuhamies'), date(2020, 2, 22), 4).
memo_score(noun('menestys'), date(2020, 2, 22), 3).
memo_score(noun('menetys'), date(2020, 2, 22), 4).
memo_score(noun('pakkanen'), date(2020, 2, 22), 4).
memo_score(noun('haastattelu'), date(2020, 2, 22), 1).
memo_score(noun('takki'), date(2020, 2, 22), 4).
memo_score(noun('paita'), date(2020, 2, 22), 5).
memo_score(noun('ohikulkija'), date(2020, 2, 22), 3).
memo_score(noun('väline'), date(2020, 2, 22), 4).
memo_score(noun('urheiluväline'), date(2020, 2, 22), 4).
memo_score(noun('saatavuus'), date(2020, 2, 22), 3).
memo_score(noun('veloitus'), date(2020, 2, 22), 0).
memo_score(noun('valokuitu'), date(2020, 2, 22), 4).

memo_score(verb('omistaa'), date(2020, 2, 22), 4).
memo_score(verb('kantaa'), date(2020, 2, 22), 4).
memo_score(verb('levitä'), date(2020, 2, 22), 4).
memo_score(verb('osallistua'), date(2020, 2, 22), 4).
memo_score(verb('jännittää'), date(2020, 2, 22), 4).
memo_score(adj('tavallinen'), date(2020, 2, 22), 4).
memo_score(adj('vapaaehtoinen'), date(2020, 2, 22), 4).
memo_score(noun('lakimies'), date(2020, 2, 22), 4).
memo_score(noun('menestys'), date(2020, 2, 22), 4).
memo_score(noun('haastattelu'), date(2020, 2, 22), 4).
memo_score(noun('ohikulkija'), date(2020, 2, 22), 4).
memo_score(noun('saatavuus'), date(2020, 2, 22), 4).
memo_score(noun('veloitus'), date(2020, 2, 22), 4).

memo_score(adj('uudenveroinen'), date(2020, 2, 24), 5).
memo_score(noun('kulmakarva'), date(2020, 2, 24), 0).
memo_score(noun('nenä'), date(2020, 2, 24), 3).
memo_score(noun('suu'), date(2020, 2, 24), 4).
memo_score(noun('hammas'), date(2020, 2, 24), 4).
memo_score(noun('huuli'), date(2020, 2, 24), 3).
memo_score(noun('parta'), date(2020, 2, 24), 4).
memo_score(noun('niska'), date(2020, 2, 24), 3).
memo_score(noun('olkapää'), date(2020, 2, 24), 3).
memo_score(noun('kyynärpää'), date(2020, 2, 24), 2).
memo_score(noun('ranne'), date(2020, 2, 24), 4).
memo_score(noun('rinta'), date(2020, 2, 24), 4).
memo_score(noun('polvi'), date(2020, 2, 24), 2).
memo_score(noun('varvas'), date(2020, 2, 24), 2).
memo_score(noun('varvas'), date(2020, 2, 24), 2).

memo_score(noun('kulmakarva'), date(2020, 2, 24), 3).
memo_score(noun('nenä'), date(2020, 2, 24), 4).
memo_score(noun('huuli'), date(2020, 2, 24), 4).
memo_score(noun('niska'), date(2020, 2, 24), 1).
memo_score(noun('olkapää'), date(2020, 2, 24), 4).
memo_score(noun('kyynärpää'), date(2020, 2, 24), 4).
memo_score(noun('polvi'), date(2020, 2, 24), 4).
memo_score(noun('varvas'), date(2020, 2, 24), 4).

memo_score(noun('kulmakarva'), date(2020, 2, 24), 4).
memo_score(noun('niska'), date(2020, 2, 24), 4).

memo_score(conj('kunnes'), date(2020, 2, 25), 5).
memo_score(conj('mikäli'), date(2020, 2, 25), 5).
memo_score(conj('jotta'), date(2020, 2, 25), 1).
memo_score(phrase('omin voimin'), date(2020, 2, 25), 4).
memo_score(verb('antaa'), date(2020, 2, 25), 4).
memo_score(verb('jää'), date(2020, 2, 25), 4).
memo_score(verb('muokata'), date(2020, 2, 25), 5).
memo_score(verb('kumota'), date(2020, 2, 25), 4).
memo_score(verb('ladata'), date(2020, 2, 25), 4).
memo_score(verb('tyhjentää'), date(2020, 2, 25), 3).
memo_score(verb('kutsua'), date(2020, 2, 25), 3).
memo_score(verb('korostaa'), date(2020, 2, 25), 3).
memo_score(verb('viettää'), date(2020, 2, 25), 4).
memo_score(verb('haitata'), date(2020, 2, 25), 4).
memo_score(verb('jutella'), date(2020, 2, 25), 4).
memo_score(verb('laittaa'), date(2020, 2, 25), 3).
memo_score(adv('heti'), date(2020, 2, 25), 4).
memo_score(adv('ensin'), date(2020, 2, 25), 5).
memo_score(adv('kuitenkin'), date(2020, 2, 25), 5).
memo_score(adv('paitsi'), date(2020, 2, 25), 4).
memo_score(adv('innoissaan'), date(2020, 2, 25), 4).
memo_score(adv('yleensä'), date(2020, 2, 25), 4).
memo_score(adv('etukäteen'), date(2020, 2, 25), 5).
memo_score(adv('nykyään'), date(2020, 2, 25), 5).
memo_score(adv('mielellään'), date(2020, 2, 25), 5).
memo_score(adj('eri'), date(2020, 2, 25), 5).
memo_score(adj('nätti'), date(2020, 2, 25), 4).
memo_score(adj('liittyvä'), date(2020, 2, 25), 2).
memo_score(adj('yleinen'), date(2020, 2, 25), 5).
memo_score(adj('kirkas'), date(2020, 2, 25), 3).
memo_score(adj('virallinen'), date(2020, 2, 25), 4).
memo_score(adj('eronnut'), date(2020, 2, 25), 4).
memo_score(adj('naimisissa'), date(2020, 2, 25), 4).
memo_score(adj('tälläinen'), date(2020, 2, 25), 4).
memo_score(adj('ylimääräinen'), date(2020, 2, 25), 4).
memo_score(adj('kamala'), date(2020, 2, 25), 5).
memo_score(adj('tyytyväinen'), date(2020, 2, 25), 4).
memo_score(adj('satunnainen'), date(2020, 2, 25), 4).
memo_score(adj('uudenveroinen'), date(2020, 2, 25), 5).
memo_score(pron('jossa'), date(2020, 2, 25), 2).
memo_score(pron('muutama'), date(2020, 2, 25), 4).
memo_score(noun('yhteys_1'), date(2020, 2, 25), 4).
memo_score(noun('yhteys_2'), date(2020, 2, 25), 4).
memo_score(noun('perustelu'), date(2020, 2, 25), 2).
memo_score(noun('verkkosivusto'), date(2020, 2, 25), 5).
memo_score(noun('tallennustila'), date(2020, 2, 25), 5).
memo_score(noun('ohjaus'), date(2020, 2, 25), 5).
memo_score(noun('kirkkaus'), date(2020, 2, 25), 5).
memo_score(noun('taustakuva'), date(2020, 2, 25), 3).
memo_score(noun('mittanauha'), date(2020, 2, 25), 4).
memo_score(noun('vahinko'), date(2020, 2, 25), 4).
memo_score(noun('tiski'), date(2020, 2, 25), 4).
memo_score(noun('ikä'), date(2020, 2, 25), 5).
memo_score(noun('käyttöikä'), date(2020, 2, 25), 4).
memo_score(noun('valinta'), date(2020, 2, 25), 5).
memo_score(noun('ominaisuus'), date(2020, 2, 25), 4).
memo_score(noun('koru'), date(2020, 2, 25), 5).
memo_score(noun('maatila'), date(2020, 2, 25), 4).
memo_score(noun('avomies'), date(2020, 2, 25), 5).
memo_score(noun('avovaimo'), date(2020, 2, 25), 5).
memo_score(noun('puoliso'), date(2020, 2, 25), 3).
memo_score(noun('asukas'), date(2020, 2, 25), 4).
memo_score(noun('some'), date(2020, 2, 25), 4).
memo_score(noun('kustannus'), date(2020, 2, 25), 4).
memo_score(noun('vapaaehtoinen'), date(2020, 2, 25), 3).
memo_score(noun('talkootyö'), date(2020, 2, 25), 3).
memo_score(noun('astia'), date(2020, 2, 25), 4).
memo_score(noun('tavara'), date(2020, 2, 25), 5).
memo_score(noun('puuhamies'), date(2020, 2, 25), 3).
memo_score(noun('menetys'), date(2020, 2, 25), 2).
memo_score(noun('pakkanen'), date(2020, 2, 25), 4).
memo_score(noun('takki'), date(2020, 2, 25), 5).
memo_score(noun('paita'), date(2020, 2, 25), 5).
memo_score(noun('väline'), date(2020, 2, 25), 4).
memo_score(noun('urheiluväline'), date(2020, 2, 25), 4).
memo_score(noun('valokuitu'), date(2020, 2, 25), 4).
memo_score(noun('suu'), date(2020, 2, 25), 4).
memo_score(noun('hammas'), date(2020, 2, 25), 5).
memo_score(noun('parta'), date(2020, 2, 25), 5).
memo_score(noun('ranne'), date(2020, 2, 25), 5).
memo_score(noun('rinta'), date(2020, 2, 25), 4).
memo_score(interjection('kissan viikset'), date(2020, 2, 25), 4).

memo_score(conj('jotta'), date(2020, 2, 25), 3).
memo_score(verb('tyhjentää'), date(2020, 2, 25), 4).
memo_score(verb('kutsua'), date(2020, 2, 25), 4).
memo_score(verb('korostaa'), date(2020, 2, 25), 2).
memo_score(verb('laittaa'), date(2020, 2, 25), 2).
memo_score(adj('liittyvä'), date(2020, 2, 25), 4).
memo_score(adj('kirkas'), date(2020, 2, 25), 4).
memo_score(pron('jossa'), date(2020, 2, 25), 4).
memo_score(noun('perustelu'), date(2020, 2, 25), 2).
memo_score(noun('taustakuva'), date(2020, 2, 25), 4).
memo_score(noun('puoliso'), date(2020, 2, 25), 4).
memo_score(noun('vapaaehtoinen'), date(2020, 2, 25), 4).
memo_score(noun('talkootyö'), date(2020, 2, 25), 5).
memo_score(noun('puuhamies'), date(2020, 2, 25), 5).
memo_score(noun('menetys'), date(2020, 2, 25), 4).

memo_score(conj('jotta'), date(2020, 2, 25), 4).
memo_score(verb('korostaa'), date(2020, 2, 25), 2).
memo_score(verb('laittaa'), date(2020, 2, 25), 4).
memo_score(verb('korostaa'), date(2020, 2, 25), 4).

memo_score(conj('sekä - että'), date(2020, 2, 26), 4).
memo_score(conj(vaan), date(2020, 2, 26), 5).
memo_score(phrase('jotain lisättävää'), date(2020, 2, 26), 4).
memo_score(phrase('sitä paisti'), date(2020, 2, 26), 3).
memo_score(verb('perustella'), date(2020, 2, 26), 3).
memo_score(verb('liityä'), date(2020, 2, 26), 4).

memo_score(conj(vaikka), date(2020, 2, 28), 4).
memo_score(conj(sillä), date(2020, 2, 28), 2).
memo_score(conj('joko - tai'), date(2020, 2, 28), 4).
memo_score(phrase('sitä paitsi'), date(2020, 2, 28), 4).
memo_score(verb('liittyä'), date(2020, 2, 28), 5).
memo_score(verb('ohjata'), date(2020, 2, 28), 4).
memo_score(verb('omistaa'), date(2020, 2, 28), 5).
memo_score(verb('kantaa'), date(2020, 2, 28), 5).
memo_score(verb('levitä'), date(2020, 2, 28), 5).
memo_score(verb('osallistua'), date(2020, 2, 28), 5).
memo_score(verb('jännittää'), date(2020, 2, 28), 4).
memo_score(adv('joten'), date(2020, 2, 28), 2).
memo_score(adv('liikaa'), date(2020, 2, 28), 2).
memo_score(adj('ominainen'), date(2020, 2, 28), 2).
memo_score(adj('tavallinen'), date(2020, 2, 28), 3).
memo_score(adj('vapaaehtoinen'), date(2020, 2, 28), 4).
memo_score(noun('nuha'), date(2020, 2, 28), 2).
memo_score(noun('päiväunet'), date(2020, 2, 28), 2).
memo_score(noun('tausta'), date(2020, 2, 28), 3).
memo_score(noun('nauha'), date(2020, 2, 28), 2).
memo_score(noun('viikset'), date(2020, 2, 28), 2).
memo_score(noun('lakimies'), date(2020, 2, 28), 4).
memo_score(noun('menestys'), date(2020, 2, 28), 4).
memo_score(noun('haastattelu'), date(2020, 2, 28), 4).
memo_score(noun('ohikulkija'), date(2020, 2, 28), 4).
memo_score(noun('saatavuus'), date(2020, 2, 28), 4).
memo_score(noun('veloitus'), date(2020, 2, 28), 2).
memo_score(adposition('kuluessa'), date(2020, 2, 28), 2).

memo_score(conj(sillä), date(2020, 2, 28), 2).
memo_score(verb(perustella), date(2020, 2, 28), 2).
memo_score(adv(joten), date(2020, 2, 28), 3).
memo_score(adv(liikaa), date(2020, 2, 28), 3).
memo_score(adj(ominainen), date(2020, 2, 28), 3).
memo_score(adj(tavallinen), date(2020, 2, 28), 3).
memo_score(noun(nuha), date(2020, 2, 28), 4).
memo_score(noun(päiväunet), date(2020, 2, 28), 4).
memo_score(noun(tausta), date(2020, 2, 28), 4).
memo_score(noun(nauha), date(2020, 2, 28), 2).
memo_score(noun(viikset), date(2020, 2, 28), 4).
memo_score(noun(veloitus), date(2020, 2, 28), 4).
memo_score(adposition(kuluessa), date(2020, 2, 28), 4).

memo_score(verb(perustella), date(2020, 2, 28), 4).
memo_score(adv(joten), date(2020, 2, 28), 4).
memo_score(adv(liikaa), date(2020, 2, 28), 4).
memo_score(adj(ominainen), date(2020, 2, 28), 4).

memo_score(conj(vaikka), date(2020, 3, 9), 5).
memo_score(conj(kunnes), date(2020, 3, 9), 2).
memo_score(conj('sekä - että'), date(2020, 3, 9), 3).
memo_score(conj('mikäli'), date(2020, 3, 9), 3).
memo_score(phrase('sitä paitsi'), date(2020, 3, 9), 2).
memo_score(phrase('omin voimin'), date(2020, 3, 9), 4).
memo_score(phrase('mahdollisimman pian'), date(2020, 3, 9), 3).
memo_score(phrase('hallitus koolla'), date(2020, 3, 9), 3).
memo_score(verb('antaa'), date(2020, 3, 9), 4).
memo_score(verb('perustella'), date(2020, 3, 9), 4).
memo_score(verb('jää'), date(2020, 3, 9), 5).
memo_score(verb('muokata'), date(2020, 3, 9), 4).
memo_score(verb('kumota'), date(2020, 3, 9), 4).
memo_score(verb('ladata'), date(2020, 3, 9), 5).
memo_score(verb('omistaa'), date(2020, 3, 9), 4).
memo_score(verb('viettää'), date(2020, 3, 9), 5).
memo_score(verb('haitata'), date(2020, 3, 9), 5).
memo_score(verb('levitä'), date(2020, 3, 9), 5).
memo_score(verb('jutella'), date(2020, 3, 9), 5).
memo_score(verb('aiheuttaa'), date(2020, 3, 9), 5).
memo_score(adv('heti'), date(2020, 3, 9), 5).
memo_score(adv('ensin'), date(2020, 3, 9), 5).
memo_score(adv('joten'), date(2020, 3, 9), 3).
memo_score(adv('kuitenkin'), date(2020, 3, 9), 5).
memo_score(adv('paitsi'), date(2020, 3, 9), 5).
memo_score(adv('innoissaan'), date(2020, 3, 9), 4).
memo_score(adv('yleensä'), date(2020, 3, 9), 4).
memo_score(adv('liikaa'), date(2020, 3, 9), 4).
memo_score(adv('etukäteen'), date(2020, 3, 9), 4).
memo_score(adv('nykyään'), date(2020, 3, 9), 4).
memo_score(adv('mielellään'), date(2020, 3, 9), 3).
memo_score(adv('riittävästi'), date(2020, 3, 9), 3).
memo_score(adv('ajallaan'), date(2020, 3, 9), 3).
memo_score(adj('eri'), date(2020, 3, 9), 5).
memo_score(adj('nätti'), date(2020, 3, 9), 5).
memo_score(adj('liittyvä'), date(2020, 3, 9), 4).
memo_score(adj('yleinen'), date(2020, 3, 9), 5).
memo_score(adj('virallinen'), date(2020, 3, 9), 4).
memo_score(adj('ominainen'), date(2020, 3, 9), 4).
memo_score(adj('eronnut'), date(2020, 3, 9), 4).
memo_score(adj('naimisissa'), date(2020, 3, 9), 4).
memo_score(adj('tälläinen'), date(2020, 3, 9), 4).
memo_score(adj('ylimääräinen'), date(2020, 3, 9), 4).
memo_score(adj('kamala'), date(2020, 3, 9), 5).
memo_score(adj('tyytyväinen'), date(2020, 3, 9), 5).
memo_score(adj('satunnainen'), date(2020, 3, 9), 4).
memo_score(adj('uudenveroinen'), date(2020, 3, 9), 5).
memo_score(adj('ajankohtainen'), date(2020, 3, 9), 2).
memo_score(pron('jossa'), date(2020, 3, 9), 4).
memo_score(pron('muutama'), date(2020, 3, 9), 5).
memo_score(noun('yhteys_1'), date(2020, 3, 9), 5).
memo_score(noun('yhteys_2'), date(2020, 3, 9), 5).
memo_score(noun('nuha'), date(2020, 3, 9), 4).
memo_score(noun('päiväunet'), date(2020, 3, 9), 4).
memo_score(noun('verkkosivusto'), date(2020, 3, 9), 5).
memo_score(noun('tallennustila'), date(2020, 3, 9), 5).
memo_score(noun('ohjaus'), date(2020, 3, 9), 5).
memo_score(noun('kirkkaus'), date(2020, 3, 9), 5).
memo_score(noun('mittanauha'), date(2020, 3, 9), 5).
memo_score(noun('vahinko'), date(2020, 3, 9), 5).
memo_score(noun('tiski'), date(2020, 3, 9), 5).
memo_score(noun('viikset'), date(2020, 3, 9), 5).
memo_score(noun('ikä'), date(2020, 3, 9), 5).
memo_score(noun('käyttöikä'), date(2020, 3, 9), 5).
memo_score(noun('valinta'), date(2020, 3, 9), 5).
memo_score(noun('ominaisuus'), date(2020, 3, 9), 5).
memo_score(noun('koru'), date(2020, 3, 9), 5).
memo_score(noun('maatila'), date(2020, 3, 9), 5).
memo_score(noun('avomies'), date(2020, 3, 9), 5).
memo_score(noun('avovaimo'), date(2020, 3, 9), 5).
memo_score(noun('lakimies'), date(2020, 3, 9), 5).
memo_score(noun('asukas'), date(2020, 3, 9), 2).
memo_score(noun('some'), date(2020, 3, 9), 4).
memo_score(noun('kustannus'), date(2020, 3, 9), 4).
memo_score(noun('astia'), date(2020, 3, 9), 5).
memo_score(noun('tavara'), date(2020, 3, 9), 5).
memo_score(noun('menetys'), date(2020, 3, 9), 4).
memo_score(noun('pakkanen'), date(2020, 3, 9), 4).
memo_score(noun('takki'), date(2020, 3, 9), 5).
memo_score(noun('paita'), date(2020, 3, 9), 5).
memo_score(noun('väline'), date(2020, 3, 9), 2).
memo_score(noun('urheiluväline'), date(2020, 3, 9), 2).
memo_score(noun('veloitus'), date(2020, 3, 9), 3).
memo_score(noun('valokuitu'), date(2020, 3, 9), 4).
memo_score(noun('kulmakarva'), date(2020, 3, 9), 2).
memo_score(noun('nenä'), date(2020, 3, 9), 3).
memo_score(noun('suu'), date(2020, 3, 9), 4).
memo_score(noun('hammas'), date(2020, 3, 9), 5).

memo_score(conj('kunnes'), date(2020, 3, 11), 2).
memo_score(conj('vaan'), date(2020, 3, 11), 4).
memo_score(phrase('jotain lisättävää'), date(2020, 3, 11), 4).
memo_score(phrase('sitä paitsi'), date(2020, 3, 11), 4).
memo_score(phrase('mahdollisimman pian'), date(2020, 3, 11), 4).
memo_score(phrase('hallitus koolla'), date(2020, 3, 11), 4).
memo_score(verb('jäädä'), date(2020, 3, 11), 5).
memo_score(verb('tyhjentää'), date(2020, 3, 11), 5).
memo_score(verb('kutsua'), date(2020, 3, 11), 5).
memo_score(verb('kantaa'), date(2020, 3, 11), 5).
memo_score(verb('osallistua'), date(2020, 3, 11), 5).
memo_score(verb('aiheuttaa'), date(2020, 3, 11), 5).
memo_score(verb('jakaa'), date(2020, 3, 11), 3).
memo_score(verb('arpoa'), date(2020, 3, 11), 4).
memo_score(verb('hylätä'), date(2020, 3, 11), 1).
memo_score(verb('päättää'), date(2020, 3, 11), 4).
memo_score(adv('riittävästi'), date(2020, 3, 11), 4).
memo_score(adv('ajallaan'), date(2020, 3, 11), 4).
memo_score(adv('ajallaan'), date(2020, 3, 11), 4).
memo_score(adj('kirkas'), date(2020, 3, 11), 5).
memo_score(adj('tälläinen'), date(2020, 3, 11), 5).
memo_score(adj('vapaaehtoinen'), date(2020, 3, 11), 5).
memo_score(adj('ajankohtainen'), date(2020, 3, 11), 5).
memo_score(noun('taustakuva'), date(2020, 3, 11), 5).
memo_score(noun('viikset'), date(2020, 3, 11), 5).
memo_score(noun('puoliso'), date(2020, 3, 11), 5).
memo_score(noun('asukas'), date(2020, 3, 11), 5).
memo_score(noun('vapaaehtoinen'), date(2020, 3, 11), 5).
memo_score(noun('talkootyö'), date(2020, 3, 11), 5).
memo_score(noun('puuhamies'), date(2020, 3, 11), 4).
memo_score(noun('haastattelu'), date(2020, 3, 11), 4).
memo_score(noun('väline'), date(2020, 3, 11), 4).
memo_score(noun('urheiluväline'), date(2020, 3, 11), 5).
memo_score(noun('veloitus'), date(2020, 3, 11), 5).
memo_score(noun('kulmakarva'), date(2020, 3, 11), 4).
memo_score(noun('huuli'), date(2020, 3, 11), 1).
memo_score(noun('parta'), date(2020, 3, 11), 4).
memo_score(noun('niska'), date(2020, 3, 11), 2).
memo_score(noun('olkapää'), date(2020, 3, 11), 4).
memo_score(noun('kyyrnapää'), date(2020, 3, 11), 4).
memo_score(noun('ranne'), date(2020, 3, 11), 5).
memo_score(noun('rinta'), date(2020, 3, 11), 5).
memo_score(noun('polvi'), date(2020, 3, 11), 4).
memo_score(noun('mahtavuus'), date(2020, 3, 11), 2).
memo_score(noun('sielu'), date(2020, 3, 11), 4).
memo_score(noun('tyyny'), date(2020, 3, 11), 4).
memo_score(noun('jakkara'), date(2020, 3, 11), 5).
memo_score(noun('valikko'), date(2020, 3, 11), 2).
memo_score(noun('valintaikkuna'), date(2020, 3, 11), 4).
memo_score(noun('valuutta'), date(2020, 3, 11), 2).
memo_score(noun('laajennus'), date(2020, 3, 11), 2).
memo_score(noun('yksityisyys'), date(2020, 3, 11), 4).
memo_score(noun('joukkoliikenne'), date(2020, 3, 11), 4).
memo_score(noun('oikotie'), date(2020, 3, 11), 5).
memo_score(verb('luoda'), date(2020, 3, 11), 5).
memo_score(noun('päätös'), date(2020, 3, 11), 4).
memo_score(noun('tammikuu'), date(2020, 3, 11), 4).
memo_score(noun('helmikuu'), date(2020, 3, 11), 4).
memo_score(noun('maaliskuu'), date(2020, 3, 11), 4).
memo_score(noun('huhtikuu'), date(2020, 3, 11), 4).
memo_score(noun('toukokuu'), date(2020, 3, 11), 4).
memo_score(noun('kesäkuu'), date(2020, 3, 11), 4).
memo_score(noun('heinäkuu'), date(2020, 3, 11), 4).
memo_score(noun('elokuu'), date(2020, 3, 11), 4).
memo_score(noun('syyskuu'), date(2020, 3, 11), 4).
memo_score(noun('lokakuu'), date(2020, 3, 11), 4).
memo_score(noun('marraskuu'), date(2020, 3, 11), 4).
memo_score(noun('joulukuu'), date(2020, 3, 11), 4).
memo_score(adposition('kuluessa'), date(2020, 3, 11), 3).
memo_score(interjection('kissan viikset'), date(2020, 3, 11), 4).

memo_score(conj(kunnes), date(2020, 3, 20), 2).
memo_score(conj('joko - tai'), date(2020, 3, 20), 4).
memo_score(conj('jotta'), date(2020, 3, 20), 4).
memo_score(conj('sitä paitsi'), date(2020, 3, 20), 4).
memo_score(phrase('mahdollisimman pian'), date(2020, 3, 20), 4).
memo_score(phrase('hallitus koolla'), date(2020, 3, 20), 4).
memo_score(verb('perustella'), date(2020, 3, 20), 5).
memo_score(verb('jäädä'), date(2020, 3, 20), 5).
memo_score(verb('liittyä'), date(2020, 3, 20), 5).
memo_score(verb('ohjata'), date(2020, 3, 20), 5).
memo_score(verb('jännittää'), date(2020, 3, 20), 5).
memo_score(verb('aiheuttaa'), date(2020, 3, 20), 5).
memo_score(verb('jakaa'), date(2020, 3, 20), 5).
memo_score(verb('arpoa'), date(2020, 3, 20), 5).
memo_score(verb('luoda'), date(2020, 3, 20), 5).
memo_score(verb('hylätä'), date(2020, 3, 20), 5).
memo_score(verb('päättää'), date(2020, 3, 20), 5).
memo_score(verb('kellua'), date(2020, 3, 20), 5).
memo_score(adv('joten'), date(2020, 3, 20), 2).
memo_score(adv('liikaa'), date(2020, 3, 20), 4).
memo_score(adv('riittävästi'), date(2020, 3, 20), 4).
memo_score(adj('ominainen'), date(2020, 3, 20), 4).
memo_score(adj('ajankohtainen'), date(2020, 3, 20), 4).
memo_score(adj('linjoilla'), date(2020, 3, 20), 4).
memo_score(noun('nuha'), date(2020, 3, 20), 5).
memo_score(noun('päiväunet'), date(2020, 3, 20), 5).
memo_score(noun('viikset'), date(2020, 3, 20), 4).
memo_score(noun('asukas'), date(2020, 3, 20), 4).
memo_score(noun('menestys'), date(2020, 3, 20), 4).
memo_score(noun('ohikulkija'), date(2020, 3, 20), 4).
memo_score(noun('väline'), date(2020, 3, 20), 4).
memo_score(noun('urheiluväline'), date(2020, 3, 20), 4).
memo_score(noun('saatavuus'), date(2020, 3, 20), 5).
memo_score(noun('veloitus'), date(2020, 3, 20), 5).
memo_score(noun('kulmakarva'), date(2020, 3, 20), 4).
memo_score(noun('huuli'), date(2020, 3, 20), 4).
memo_score(noun('niska'), date(2020, 3, 20), 4).
memo_score(noun('kyynärpää'), date(2020, 3, 20), 4).
memo_score(noun('sielu'), date(2020, 3, 20), 4).
memo_score(noun('tyyny'), date(2020, 3, 20), 4).
memo_score(noun('jakkara'), date(2020, 3, 20), 4).
memo_score(noun('valikko'), date(2020, 3, 20), 3).
memo_score(noun('valintaikkuna'), date(2020, 3, 20), 3).
memo_score(noun('valuutta'), date(2020, 3, 20), 2).
memo_score(noun('laajennus'), date(2020, 3, 20), 4).
memo_score(noun('yksityisyys'), date(2020, 3, 20), 4).
memo_score(noun('joukkoliikenne'), date(2020, 3, 20), 4).
memo_score(noun('oikotie'), date(2020, 3, 20), 4).
memo_score(noun('päätös'), date(2020, 3, 20), 4).
memo_score(noun('tammikuu'), date(2020, 3, 20), 4).
memo_score(noun('helmikuu'), date(2020, 3, 20), 5).
memo_score(noun('maaliskuu'), date(2020, 3, 20), 5).
memo_score(noun('huhtikuu'), date(2020, 3, 20), 5).
memo_score(noun('toukokuu'), date(2020, 3, 20), 4).
memo_score(noun('kesäkuu'), date(2020, 3, 20), 5).
memo_score(noun('heinäkuu'), date(2020, 3, 20), 4).
memo_score(noun('elokuu'), date(2020, 3, 20), 3).
memo_score(noun('syyskuu'), date(2020, 3, 20), 4).
memo_score(noun('lokakuu'), date(2020, 3, 20), 3).
memo_score(noun('marraskuu'), date(2020, 3, 20), 4).
memo_score(noun('joulukuu'), date(2020, 3, 20), 5).
memo_score(noun('harrastus'), date(2020, 3, 20), 5).
memo_score(noun('kokoaminen'), date(2020, 3, 20), 5).
memo_score(noun('torkku'), date(2020, 3, 20), 4).
memo_score(noun('voimassaolo'), date(2020, 3, 20), 4).
memo_score(adposition('kuluessa'), date(2020, 3, 20), 2).

memo_score(conj('kunnes'), date(2020, 3, 20), 4).
memo_score(conj('sekä - että'), date(2020, 3, 20), 4).
memo_score(conj('mikäli'), date(2020, 3, 20), 4).
memo_score(adv('joten'), date(2020, 3, 20), 2).
memo_score(adv('mielellään'), date(2020, 3, 20), 4).
memo_score(noun('nenä'), date(2020, 3, 20), 4).
memo_score(noun('mahtavuus'), date(2020, 3, 20), 4).
memo_score(noun('valikko'), date(2020, 3, 20), 5).
memo_score(noun('valintaikkuna'), date(2020, 3, 20), 5).
memo_score(noun('valuutta'), date(2020, 3, 20), 3).
memo_score(noun('elokuu'), date(2020, 3, 20), 2).
memo_score(noun('lokakuu'), date(2020, 3, 20), 4).
memo_score(adposition('kuluessa'), date(2020, 3, 20), 4).

memo_score(noun('valuutta'), date(2020, 3, 20), 4).
memo_score(noun('elokuu'), date(2020, 3, 20), 5).

memo_score(conj(eli), date(2020, 3, 21), 5).
memo_score(conj('ts.'), date(2020, 3, 21), 0).
memo_score(phrase('sitä paitsi'), date(2020, 3, 21), 5).
memo_score(verb(kellua), date(2020, 3, 21), 5).
memo_score(verb(tutustua), date(2020, 3, 21), 4).
memo_score(verb(päästä), date(2020, 3, 21), 2).
memo_score(verb(joutua), date(2020, 3, 21), 1).
memo_score(verb(todeta), date(2020, 3, 21), 1).
memo_score(verb(vaatia), date(2020, 3, 21), 1).
memo_score(verb(esittää), date(2020, 3, 21), 4).
memo_score(verb(aloittaa), date(2020, 3, 21), 4).
memo_score(verb(kasvaa), date(2020, 3, 21), 4).
memo_score(verb(arvioida), date(2020, 3, 21), 5).
memo_score(verb(tuntea), date(2020, 3, 21), 5).
memo_score(verb(lisätä), date(2020, 3, 21), 5).
memo_score(verb(jättää), date(2020, 3, 21), 1).
memo_score(verb(yrittää), date(2020, 3, 21), 4).
memo_score(adv(jopa), date(2020, 3, 21), 4).
memo_score(adv(taas), date(2020, 3, 21), 5).
memo_score(adv('mm.'), date(2020, 3, 21), 5).
memo_score(adv('saman tien'), date(2020, 3, 21), 5).
memo_score(adv('edelleen'), date(2020, 3, 21), 1).
memo_score(adv('miten'), date(2020, 3, 21), 1).
memo_score(adj(linjoilla), date(2020, 3, 21), 4).
memo_score(adj(tärkeä), date(2020, 3, 21), 4).
memo_score(adj(entinen), date(2020, 3, 21), 4).
memo_score(adj(kasainvälinen), date(2020, 3, 21), 1).
memo_score(pron(jokin), date(2020, 3, 21), 5).
memo_score(noun(harrastus), date(2020, 3, 21), 5).
memo_score(noun(kokoaminen), date(2020, 3, 21), 5).
memo_score(noun(torkku), date(2020, 3, 21), 5).
memo_score(noun(voimassaolo), date(2020, 3, 21), 5).
memo_score(noun(välilehti), date(2020, 3, 21), 5).
memo_score(noun(vihollinen), date(2020, 3, 21), 5).
memo_score(noun(kisa), date(2020, 3, 21), 4).
memo_score(noun(syy), date(2020, 3, 21), 4).
memo_score(noun(kilpailu), date(2020, 3, 21), 4).
memo_score(noun(joukkue), date(2020, 3, 21), 4).
memo_score(noun(valtio), date(2020, 3, 21), 4).
memo_score(noun(joukko), date(2020, 3, 21), 4).
memo_score(noun(tavoite), date(2020, 3, 21), 1).
memo_score(noun(voitto), date(2020, 3, 21), 1).
memo_score(noun(ottelu), date(2020, 3, 21), 1).
memo_score(noun(yhteistyö), date(2020, 3, 21), 3).
memo_score(noun(tutkimus), date(2020, 3, 21), 4).
memo_score(adposition(aikana), date(2020, 3, 21), 4).

memo_score(conj('ts.'), date(2020, 3, 21), 3).
memo_score(verb(päästä), date(2020, 3, 21), 4).
memo_score(verb(joutua), date(2020, 3, 21), 4).
memo_score(verb(todeta), date(2020, 3, 21), 4).
memo_score(verb(vaatia), date(2020, 3, 21), 2).
memo_score(verb(jättää), date(2020, 3, 21), 4).
memo_score(adv(edelleen), date(2020, 3, 21), 4).
memo_score(adv(miten), date(2020, 3, 21), 2).
memo_score(noun(tavoite), date(2020, 3, 21), 2).
memo_score(noun(voitto), date(2020, 3, 21), 4).
memo_score(noun(ottelu), date(2020, 3, 21), 1).
memo_score(noun(yhtyistyö), date(2020, 3, 21), 4).

memo_score(conj('ts.'), date(2020, 3, 21), 4).
memo_score(verb(vaatia), date(2020, 3, 21), 2).
memo_score(adv(miten), date(2020, 3, 21), 4).
memo_score(noun(tavoite), date(2020, 3, 21), 4).
memo_score(noun(yhteistyö), date(2020, 3, 21), 4).

% SUPERMEMO-2 https://www.supermemo.com/en/archives1990-2015/english/ol/sm2

study :- forall(next(E), show_entry(E)).
study_below_four :- forall(below_four(E), show_entry(E)).

show_entry(E) :- writeln(E), get_char(_), meaning(E, M), writeln(M), get_char(_).

next(Entry) :-
    entry_deadline(Entry, DeadLine),
    date(Today),
    date_interval(Today, DeadLine, DaysPastDeadline days),
    DaysPastDeadline >= 0.

below_four(Entry) :-
    find_entry(Entry),
    latest_memo_nth(Entry, N),
    nth_score(Entry, N, _, Score),
    Score < 4.

find_entry(Entry) :-
    entry_type(Type), Entry =.. [Type, _], call(Entry).

entry_deadline(Entry, DeadLine) :-
    find_entry(Entry),
    latest_memo_interval(Entry, N, RecommendedStudyInterval),
    (nth_score(Entry, N, LastDate, _); date(Today), LastDate = Today),
    RoundedInterval is round(RecommendedStudyInterval),
    date_add(LastDate, RoundedInterval days, DeadLine).

entries_deadline(Es, D) :-
    setof(E, entry_deadline(E, D), Es).

latest_memo_interval(Entry, N, I) :-
    latest_memo_nth(Entry, N),
    memo_interval(Entry, N, I).

memo_interval(_, 0, 0) :- !.
memo_interval(_, 1, 1) :- !.
memo_interval(_, 2, 6) :- !.
memo_interval(Entry, Nth, Days) :-
    PrevNth is Nth - 1,
    PrevNth >= 0,
    nth_score(Entry, Nth, _, Score),
(   Score < 3 ->
    Days is 1
;
    ef(Entry, Nth, EF),
    memo_interval(Entry, PrevNth, PrevI),
    Days is EF * PrevI
).

ef(_, 1, 2.5) :- !.
ef(Entry, Nth, EF) :-
%    EF':=EF+(0.1-(5-q)*(0.08+(5-q)*0.02))
    PrevNth is Nth - 1,
    PrevNth >= 0,
    nth_score(Entry, PrevNth, _, PrevScore),
    ef(Entry, PrevNth, PrevEF),
    RawEF is PrevEF + (0.1 - (5 - PrevScore) * (0.08 + (5 - PrevScore) * 0.02)),
(   RawEF > 1.3 ->
    EF = RawEF
;
    EF = 1.3
).

nth_score(Entry, Nth, Date, Score) :-
    setof([Date, Score], Date^memo_score(Entry, Date, Score), Scores),
    nth1(Nth, Scores, [Date, Score]).

latest_memo_nth(Entry, Nth) :-
    aggregate_all(count, memo_score(Entry, _, _), Nth).

study_agg(Date, Count) :- aggregate(count, (E-S)^memo_score(E, Date, S), Count).
deadline_agg(Date, Count) :- aggregate(count, E^entry_deadline(E, Date), Count).

date_deadline_studied(D, C, C2) :- study_agg(D, C), deadline_agg(D, C2).
date_deadline_studied(D, C, 0) :- study_agg(D, C), \+ deadline_agg(D, _).
date_deadline_studied(D, 0, C2) :- deadline_agg(D, C2), \+ study_agg(D, _).

export_chart_data(Path) :-
    open(Path, write, Stream),
    forall(date_deadline_studied(D, C, C2), (format_time(string(Date), '%Y-%m-%d', D), csv_write_stream(Stream, [row(Date, C, C2)], []))),
    close(Stream).

due_today_count(C) :-
    aggregate_all(count, next(_), C).
