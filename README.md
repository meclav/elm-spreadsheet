# elm-spreadsheet
Spreadsheet widget for Elm. Since both Elm and Excel are "zero order functional languages" it should be easy to implement one in the other. So the vision is to make an editable grid that accepts Excel formulas and let you prepare a component that is then tweaked or further edited by the client in the browser. There are heavyweight proprietary products that do in-browser Excel and Google docs and Angular grids. I'd like something that has signals coming out which then can feed into charts etc.

State:
Working on tokenising and parsing the formulas. Got some skeletal sketch of the whole. Displaying, initializing, connecting signals out - not done. 
So it's all not ready yet and some implemented bits are more skeletal than others. Get in touch if you'd like to collaborate on this.




Some materials and dead ends: 

This K file has the grammar and the precendences worked out. Don't worry about every symbol; feel the intent.
http://www.nsl.com/k/excel.k

This guy did a tokenizer with a state table and some ad hoc Excel rules, the Javascript is fairly clear.
http://ewbi.blogs.com/develops/2004/12/excel_formula_p.html

This is how you can represent a graph inductively and then pattern match. Makes depth-first search easy, but changing the graph hard.
https://web.engr.oregonstate.edu/~erwig/papers/InductiveGraphs_JFP01.pdf
14/12/2015: Exciting stuff, I've realised someone ported FGL to Elm : https://github.com/sgraf812/elm-graph

