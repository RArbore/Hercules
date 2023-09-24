%x comment
%%
/\*    <+comment>;

<comment>/\*  <+comment>;
<comment>\*+/ <-comment>;
<comment>\*+  ;
<comment>.    ;

[0-9]+ "INT"
\+     "+"
\*     "*"
\(     "("
\)     ")"
[\t ]+ ;
.      "UNRECOGNIZED"
