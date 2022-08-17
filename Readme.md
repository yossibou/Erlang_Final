git instruction

1. mkdir git
2. cd git
3. git clone https://github.com/yossibou/Erlang_Final.git

run instruction

(same in multiple computer or on single computer - for single computer use same ip or loopback)

1. Insert into the file computer.hrl the IP addresses of the secondary computers in the following manner:
-define(PC1, 'PC1@IP_ADDRESS1').
-define(PC2, 'PC2@IP_ADDRESS2').
-define(PC3, 'PC3@IP_ADDRESS3').
-define(PC4, 'PC4@IP_ADDRESS4').
-define(MASTER,'home@IP_ADDRESS5').

2. On master computer run erl -setcookie dough -name home@IP_ADDRESS5
3. For each other host run erl -setcookie dough -name PCn@IP_ADDRESS

4. on all computer run lc([ride,child,gui,generator,host,master]).
5. on master computer run : master:start().

Youtube link: https://youtu.be/gTg5Qeay6Ic
