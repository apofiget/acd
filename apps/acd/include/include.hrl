
-include_lib("kernel/include/file.hrl").

-define(DEFCONF, [
		{yaws_host,"localhost"},
		{tty, "/dev/ttyACM0"},
		{tty_speed, 9600},
		{tty_timeout, 10000}
		]).

-define(MONTH,[{1,"January"}, {2,"February"}, {3,"March"}, {4,"April"}, 
			   {5,"May"}, {6,"June"}, {7,"Jule"}, {8,"August"}, {9,"September"}, 
			   {10,"October"}, {11,"November"}, {12,"December"}]).
