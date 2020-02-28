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

meaning(verb(antaa), 'to give').

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

meaning(verb(esiintyä), 'occur').

meaning(noun(yhteys_1), 'connection').
meaning(noun(yhteys_2), 'context').

meaning(noun(perustelu), 'reasoning').

meaning(verb(perustella), 'to argue, justify, explain').
comes_from(noun(perustelu), verb(perustella)).

meaning(phrase('jotain lisättävää'), 'something to add (to a conversation)').

meaning(noun(nuha), 'runny nose').
example(noun(nuha), 'Minulla on nuha ja olen nukkunut huonosti.').

meaning(noun(päiväunet), 'nap').
example(noun(päiväunet), 'Olen sekä nukkunut päiväunet että juonut kuumaa mehua.').

meaning(conj('joko - tai'), 'either - or').
example(conj('joko - tai'), 'Me ostamme joko kissan tai koiran.').

meaning(verb(levätä), 'to rest').
example(verb(levätä), 'Lepään kotona').

meaning(verb(jää), 'to stay').
example(verb(jää), 'En mene tänään töihin vaan jään kotiin').

meaning(adv(innoissaan), 'excitedly').
example(adv(innoissaan), '"Kivaa", hän sanoi innoissaan.').
comes_from(adv(innoissaan), verb(innostaa)).

meaning(verb(innostaa), 'to excite, inspire').
example(verb(innostaa), 'Mikä innostaa sinua?').

meaning(phrase('sitä paitsi'), 'besides, in addition to (that)').
example(phrase('sitä paitsi'), 'Sitä paitsi olen vanhempi kuin sinä.').

meaning(adj(liittyvä), 'related').
example(adj(liittyvä), 'liittyvät tehtävät').

comes_from(adj(liittyvä), verb(liittyä)).

meaning(verb(liittyä), 'to join (a group), to be related to, to be part of').

meaning(verb(muokata), 'to edit/modify').

meaning(adj(yleinen), 'general').

meaning(adv(yleensä), 'usually, generally').
example(adv(yleensä), 'He menevät yleensä ulos lauantaisin.').

meaning(noun(verkkosivusto), 'website').
etymology(noun(verkkosivusto), 'verkko (net) + sivu (page) + -sto (place)').

meaning(noun(viesti), 'message').

meaning(verb(kumota), 'to undo, cancel').

meaning(noun(tallennustila), 'storage space').

meaning(noun(tausta), 'background').

meaning(noun(ohjaus), 'control').
comes_from(noun(ohjaus), verb(ohjata)).

meaning(verb(ohjata), 'control').

meaning(noun(kirkkaus), 'brightness').
comes_from(noun(kirkkaus), adj(kirkas)).

meaning(adj(kirkas), 'bright').

meaning(noun(taustakuva), 'wallpaper').

meaning(noun(nauha), 'tape, ribbon, band, string').

meaning(noun(mittanauha), 'measuring tape').

meaning(verb(ladata), 'to download').

meaning(verb(tyhjentää), 'to empty, to reset').

meaning(conj(mikäli), 'if, provided that, in the event that').
example(conj(mikäli), 'Mikäli tämän ajan kuluessa varausta ei makseta, varauksenne peruuntuu.').

meaning(noun(vahinko), 'mishap, accident, damage').

meaning(adv(liikaa), 'too much').
example(adv(liikaa), 'Elämässä ei ole koskaan liikaa laulua ja hyvää seuraa.').

meaning(verb(kutsua), 'to invite').

meaning(noun(tiski), 'counter (bar)').
example(noun(tiski), 'tehdä tilauksesi tiskillä').

meaning(adv(etukäteen), 'in advance').
etymology(adv(etukäteen), 'etu- (“fore-”) +‎ käteen (“hand”)').

meaning(adposition(kuluessa), 'within').
example(adposition(kuluessa), 'He soittavat meille viikon kuluessa.').

meaning(interjection('kissan viikset'), 'nonsense, rubbish').

meaning(noun(viikset), 'mustache').

meaning(verb(korostaa), 'emphasize, highlight').

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

meaning(verb(omistaa), 'to own').
example(verb(omistaa), 'Minä omistan talon ja maatilan.').

meaning(noun(maatila), 'farm').

meaning(adj(eronnut), 'divorced').

meaning(verb(viettää), 'to spend time, to pass the time').

meaning(adj(naimisissa), 'married').

meaning(noun(avomies), 'husband').
meaning(noun(avovaimo), 'wife').
meaning(noun(puoliso), 'spouse').

meaning(noun(lakimies), 'lawyer').

meaning(verb(kantaa), 'to carry').
example(verb(kantaa), 'Tom kantaa sinua.').

meaning(noun(asukas), 'resident').

meaning(verb(haitata), 'to bother, to hinder').
example(verb(haitata), 'Kylmä ei haittaa ketään').

meaning(adv(nykyään), 'recently, nowadays').

meaning(adj(tavallinen), 'usual, ordinary, common, trivial, standard').

meaning(phrase('omin voimin'), 'on your/their own').

meaning(adj(tällainen), 'such, this kind of').

meaning(noun(some), 'social media').

meaning(verb(levitä), 'to spread (words)').
example(verb(levitä), 'sana leviää nopeasti ja helposti').

meaning(noun(kustannus), 'cost, expense').

meaning(noun(vapaaehtoinen), 'a volunteer').
meaning(adj(vapaaehtoinen), 'voluntary').

meaning(verb(osallistua), 'to participate, to attend').

meaning(adv(mielellään), 'gladly, willingly').

meaning(noun(talkootyö), 'voluteer work').

meaning(noun(astia), 'dish, plate').

meaning(adj(ylimääräinen), 'excessive, redundant').

meaning(noun(tavara), 'goods, wares, belongings').

meaning(noun(puuhamies), 'organizer, especially one who works on a voluntary basis.').

meaning(noun(menestys), 'success').
meaning(noun(menetys), 'loss').

meaning(verb(jännittää), 'to tense up, to feel excited and nervous').
example(verb(jännittää), 'Mä muistan hyvin, miten mua jännitti.').

meaning(adj(kamala), 'horrible').
meaning(noun(pakkanen), 'frost').

meaning(adj(tyytyväinen), 'content, happy').

meaning(noun(haastattelu), 'an interview').

meaning(pron(muutama), 'a few, some').
example(pron(muutama), 'Oscar myy kolme kirja muutamalla eurolla').

meaning(verb(jutella), 'to chat').
example(verb(jutella), 'asiakas juttele hetken').

meaning(noun(takki), 'jacket, coat').
meaning(noun(paita), 'shirt').

meaning(adj(satunnainen), 'random').
meaning(noun(ohikulkija), 'passer-by').

meaning(noun(väline), 'tool, equipment').
meaning(noun(urheiluväline), 'sports equipment').

meaning(verb(laittaa), 'put, set, place').
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

meaning(verb(aiheuttaa), 'to cause, to lead to, to result in').
example(verb(aiheuttaa), 'aiheuttaa hälytyksen').

meaning(adj(ajankohtainen), 'current, of interest (situation)').

meaning(adv(ajallaan), 'on time').

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
memo_score(verb(jää), date(2020, 2, 20), 4).
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
memo_score(adj('tällainen'), date(2020, 2, 22), 5).
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
memo_score(adj('tällainen'), date(2020, 2, 25), 4).
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
