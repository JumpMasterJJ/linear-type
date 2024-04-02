/* Examples for testing */


/* Syntax Check */
un true;
if un false then un true else un false;


/* Evalution Test For Unrestricted */
un true;
if un false then un true else un false;
if un true then un true else un false;
un <un true,un false>;
un lambda x:un Bool.un false;
(un lambda x:un un Bool * un Bool.split x as y1,y2 in y2) (un <un true,un false>);