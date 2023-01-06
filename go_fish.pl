
enqueue(E, Q, [E|Q]).

%remove the last element
dequeue(Q, NQ) :-
    reverse(Q, RQ),
    remove_card(H, [H|RQ], NRQ),
    reverse(NRQ, NQ)
    .

%go fish
game(H, H1) :-
    make_deck(D),
shuffle_deck(D, SD),
deal_hand(SD, H, 0),
take_remove_from_deck(7, New, SD),
deal_hand(New, H1, 0).
    %turn(H, H1).

turn(Hand, Hand1) :-
    %check for pairs
    find_pairs(Hand, Pairs, NewHand),
    %check for win
    	checkWin(NewHand, Hand1),
    %ask
	write('ask for a card rank'),
	
read(CardValue),
ask(CardValue, NewHand, Hand1, NH, NH1), 
        find_pairs(Hand, Pairs, NewHand),
    %check for win
    checkWin(NH, NH1)
    %other player turn.
%cpu_turn(NH1, NH)
    .

    
card(S, N).

card(S, N, P) :-
    random(0, 52, P),
    write(P).


checkWin([], CPU) :-
    write('player win').

checkWin(Player, []) :-
    write('CPU win').

remove_card(C, [], ND).
% remove speciifc card from deck
%card is this iter
remove_card(C, [C|D], ND) :-
    remove_card(C, D, ND).

remove_card(C, [N|D], [N|ND]) :-
    remove_card(C, D, ND).

find_pairs(Hand, Pairs, NewHand) :-
    findall((card(S1, V)), (member(card(S1, V), Hand),
                               member(card(S2, F), Hand),
                               F == V, S1 \== S2), Pairs),

    write(P),
        subtract(Hand, Pairs, NewHand).

find_pairs(Hand, Pairs, _) :-
	    findall((card(S1, V)), (member(card(S1, V), Hand), member(card(S2, F), Hand), F == V, S1 \== S2), Pairs),
    	Pairs == [],
   		write('no pairs').
/*
 * computer remembers the last six cards asked for
 * if a player asks for a card and the CPU dosent have it
 * add to the remember list
 * 
 * when list has 6 members
 * pop off the oldest one
 * */


%enqueue(Elem, Queue, NewQueue)
%dequeue(Queue, Newqueue)
remember_list(card(_, V), RL, NL) :-
   \+ length(RL, 6),
    enqueue(card(_,V), RL, NL)
    .
remember_list(card(_, V), RL, FRL) :-
    length(RL, 6),
    dequeue(RL, NRL),
    enqueue(card(_,V), NRL, FRL)
    .

%Players take turns asking the other player for a card that
%matches one they hold. Players cannot ask
%for a card if they do not hold a matching card.

%has card
ask(card(S, Value), PlayerHand, CPUHand,NP, CP) :-
    member(card(_, Value), PlayerHand),
    member(card(S, Value), CPUHand),
    append(PlayerHand, card(S,Value), NP),
    remove_card(card(S, Value), CPUHand, CP).
    %check pairs for player 1
    %find_pairs(NP, Pairs, NP1),
   % Pairs \== [].
    %turn(NP1, CP)
    %find_pairs(NP, P, NNP)
    /*If the player who is asked for a card
     * has the card, that player must give
     *  it to the person who asked for it.
That player places the pair of cards face up
with the other pairs and gets another turn*/

%doesnt have card - go fish
ask(card(_, Value), PlayerHand, CPUHand, NewHand, NewCPUHand) :-
    member(card(_, Value), PlayerHand),
    \+ member(card(_, Value), CPUHand),
     draw_1([H, _, Z|_], PlayerHand, NewHand).
   % find_pairs(NewHand, P, NH2).
    %take 1 from deck
    
/*If the player does not have the card
 *  asked for, the player takes any card
 * from the FISH POND. If that
card matches, the player places the pair
with the other pairs but does not get another turn*/

cleanup_list([], CleanList).

cleanup_list([H|L], [H|CleanList]) :-
    nonvar(H),
        cleanup_list(L, CleanList)
.
cleanup_list([H|L], CleanList) :-
    \+ nonvar(H),
    cleanup_list(L, CleanList)
    .


draw_1([H, _, Z|_], Hand, NewHand) :-
    nonvar(H),
    append(Hand, Z, NewHand).

draw_1([H|Deck], Hand, NewHand) :-
 \+nonvar(H),
    draw_1(Deck, Hand, NewHand).

make_suit(L, S, NL) :-
    append(L, [
		card(S, ace),
		card(S, 1),
                card(S, 2),
                card(S, 3),
                card(S, 4),
                card(S, 5),
                card(S, 6),
                card(S, 7),
                card(S, 8),
                card(S, 9),
                card(S, 10),
                card(S, jack),
                card(S, queen),
                card(S, king)
                ], NL).

make_deck(D) :-
    make_suit(_, heart, L1),
    make_suit(L1, diamond, L2),
    make_suit(L2, club, L3),
    make_suit(L3, spade, D).

shuffle_deck(D, SD) :-
    random_permutation(D,SD).

incr(X, X1) :-
    X1 is X+1.

deal_hand(Deck, Hand, 7).

deal_hand([H|Deck], [H|Hand], I) :-
    incr(I, I1),
    deal_hand(Deck, Hand, I1).

take_remove_from_deck(Amount, Result, OriginalDeck) :-
    length(X, Amount),
    append(X, Result, OriginalDeck).