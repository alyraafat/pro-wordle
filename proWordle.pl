main:-
	build_kb,
	play.
	
build_kb:-
	build_kb_helper1,
	build_kb_helper2.
	
build_kb_helper1:-
	write('Welcome to Pro-Wordle!'),
	nl,
	write("----------------------"),
	nl,nl.
build_kb_helper2:-
	write("Please enter a word and its category on separate lines:"),
	nl,
	read(W),
	(
		(
			nonvar(W),
			(
				(
					W \== done,
					read(C),
					(	
						(
							nonvar(C),
							assert(word(W,C)),
							build_kb_helper2
						);
						(
							\+nonvar(C),
							write('This word is not valid.'),
							nl,
							build_kb_helper2
						)
					)
				);
				(
					W==done,
					nl,
					write('Done building the words database...'),
					nl,nl
				)
			)
		);
		(
			\+nonvar(W),
			write('This word is not valid.'),
			nl,
			build_kb_helper2
		)
	).
		
play:-
	write('The available categories are: '),
	categories(L),
	write(L),
	nl,
	play_cat_guess(C),
	setof(X,word(X,C),All_Words_in_Cat),
	play_length_guess(G,All_Words_in_Cat),
	pick_word(W,G,C),
	G1 is G+1,
	play_word_guess(G,G1,W).
	

play_cat_guess(X):-
	write('Choose a category:'),
	nl,
	read(C),
	(
		nonvar(C),
		(
			(
				(
					\+is_category(C),
					write('This category does not exist.'),
					nl,
					play_cat_guess(X)
				);
				(
					is_category(C),
					X=C
				)
			)
		)
	);
	(
		\+nonvar(C),
		write('This category does not exist.'),
		nl,
		play_cat_guess(X)
	).
	
play_length_guess(X,All_Words_in_Cat):-
	write('Choose a length:'),
	nl,
	read(L),
	(
		(
			number(L),
			(
				(
					\+available_length_in_cat(L,All_Words_in_Cat),
					write('There are no words of this length.'),
					nl,
					play_length_guess(X,All_Words_in_Cat)
				);
				(
					available_length_in_cat(L,All_Words_in_Cat),
					X=L,
					write('Game started. You have '),
					Guesses is L+1,
					write(Guesses),
					write(' guesses.'),
					nl,nl
				)
			)
		);
		(
			\+number(L),
			write('Please enter a number.'),
			nl,
			play_length_guess(X,All_Words_in_Cat)
		)
	).
	
available_length_in_cat(L,[H|_]):-
	atom_chars(H,HL),
	length(HL,L).
available_length_in_cat(L,[H|T]):-
	atom_chars(H,HL),
	length(HL,L1),
	L1\==L,
	available_length_in_cat(L,T).
	
play_word_guess(L1,1,X):-
	write('Enter a word composed of '),
	write(L1),
	write(' letters:'),
	nl,
	read(W),
	(
		(
			nonvar(W),
			(
				atom_chars(W,WL),
				length(WL,L),
				(	
					(	
						L==L1,
						(
							(
								word(W,_),
								(
									(
										W==X,
										write('You won!'),
										nl
									);
									(
										W\==X,
										write('You lost!'),
										nl
									)
								)
							);
							(
								\+word(W,_),
								write('This word is not valid.'),
								nl,
								write('Remaining Guesses are 1.'),
								nl,nl,
								play_word_guess(L1,1,X)
							)
						)
					);
					(
						L\==L1,
						write('Word is not composed of '),
						write(L1),
						write(' letters. Try again!'),
						nl,
						write('Remaining Guesses are 1.'),
						nl,nl,
						play_word_guess(L1,1,X)
					)
				)
			)
		);
		(
			\+nonvar(W),
			write('This word is not valid.'),
			nl,
			write('Remaining Guesses are 1.'),
			nl,nl,
			play_word_guess(L1,1,X)
		)
	).
play_word_guess(L1,G,X):-
	G>1,
	write('Enter a word composed of '),
	write(L1),
	write(' letters:'),
	nl,
	read(W),
	(
		(
			nonvar(W),	
			(
				atom_chars(X,XL),
				atom_chars(W,WL),
				length(WL,L),
				(
					(
						L==L1,
						(
							(
								word(W,_),
								(
									(
										W==X,
										write('You won!'),
										nl
									);
									(
										W\==X,
										correct_letters(WL,XL,CL),
										write('Correct letters are: '),
										write(CL),
										nl,
										correct_positions(WL,XL,CP),
										write('Correct letters in correct positions are: '),
										write(CP),
										nl,
										write('Remaining Guesses are '),
										G1 is G-1,
										write(G1),
										write('.'),
										nl,nl,
										play_word_guess(L1,G1,X)
									)
								)
							);
							(
								\+word(W,_),
								write('This word is not valid.'),
								nl,
								write('Remaining Guesses are '),
								write(G),
								write('.'),
								nl,nl,
								play_word_guess(L1,G,X)
							)
						)
					);
					(
						L\==L1,
						write('Word is not composed of '),
						write(L1),
						write(' letters. Try again!'),
						nl,
						write('Remaining Guesses are '),
						write(G),
						write('.'),
						nl,nl,
						play_word_guess(L1,G,X)
					)
				)
			)
		);
		(
			\+nonvar(W),
			write('This word is not valid.'),
			nl,
			write('Remaining Guesses are '),
			write(G),
			write('.'),
			nl,nl,
			play_word_guess(L1,G,X)
		)
	).
		
	
is_category(C):-
	word(_,C).

categories(L):-
	setof(C,is_category(C),L).
	
available_length(L):-
	word(X,_),
	atom_chars(X,X1),
    length(X1,L),!.

pick_word(W,L,C):-
	word(X,C),
	atom_chars(X,X1),
    length(X1,L),
	W = X.

correct_letters(_,[],[]).
correct_letters(L1,[H|T],[H|T1]):-
	\+member(H,T),
	member(H,L1),
	correct_letters(L1,T,T1).
correct_letters(L1,[H|T],CL):-
	\+member(H,T),
	\+member(H,L1),
	correct_letters(L1,T,CL).
correct_letters(L1,[H|T],CL):-
	member(H,T),
	correct_letters(L1,T,CL).
	
correct_positions([],L2,[]):-length(L2,L),L>0.
correct_positions(L1,[],[]):-length(L1,L),L>0.
correct_positions([],[],[]).
correct_positions([H|T1],[H|T2],[H|T3]):-
	correct_positions(T1,T2,T3).
correct_positions([H1|T1],[H2|T2],PL):-
	H1\==H2,
	correct_positions(T1,T2,PL).