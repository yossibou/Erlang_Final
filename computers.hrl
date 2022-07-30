
-define(PC1, 'PC1@192.168.31.92').
-define(PC2, 'PC2@192.168.31.127').
-define(PC3, 'PC3@192.168.31.127').
-define(PC4, 'PC4@192.168.31.127').
-define(Master,'home@192.168.31.127').

%erl -setcookie dough -name Master@192.168.31.127
%erl -setcookie dough -name PC1@192.168.31.127
%erl -setcookie dough -name PC2@192.168.31.127
%erl -setcookie dough -name PC3@192.168.31.127
%erl -setcookie dough -name PC4@192.168.31.127
