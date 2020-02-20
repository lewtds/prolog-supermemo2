:- use_module(library(date_time)).

:- discontiguous verb/1.
:- discontiguous conj/1.
:- discontiguous phrase/1.
:- discontiguous meaning/2.
:- discontiguous example/2.
:- discontiguous comes_from/2.
:- discontiguous similar/2.

entry_type(conj).
entry_type(phrase).
entry_type(verb).
entry_type(adv).
entry_type(adj).
entry_type(pron).
entry_type(noun).

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

meaning(conj(sillä), 'since').

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

meaning(conj('joko — tai'), 'either - or').
example(conj('joko — tai'), 'Me ostamme joko kissan tai koiran.').

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

% SUPERMEMO-2 https://www.supermemo.com/en/archives1990-2015/english/ol/sm2
next(Entry) :-
    find_entry(Entry),
    latest_memo_nth(Entry, N),
    memo_interval(Entry, N, RecommendedStudyInterval),
(
    nth_score(Entry, N, LastDate, _),
    date(Today),
    date_add(LastDate, RecommendedStudyInterval, DeadLine),
    date_interval(Today, DeadLine, DaysPastDeadline days),
    DaysPastDeadline >= 0
;
    RecommendedStudyInterval = 0
).

below_four(Entry) :-
    find_entry(Entry),
    latest_memo_nth(Entry, N),
    nth_score(Entry, N, _, Score),
    Score < 4.

find_entry(Entry) :-
    entry_type(Type), Entry =.. [Type, _], call(Entry).

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
