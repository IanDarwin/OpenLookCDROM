## ###################################################################### ##
##         Copyright IBM Corporation 1988,1991 - All Rights Reserved      ##
##        For full copyright information see:'andrew/config/COPYRITE'     ##
## ###################################################################### ##

BEGIN	{{OFS = ""; ORS = "\n";
	TransCount = 0;
	}}
{
	if (HaveMap[$1 ";" $2] == 0) {
		Mapping[$1] = Mapping[$1] " " $2;
		HaveMap[$1 ";" $2] = 1;
	}
}
END	{
	for (z in Mapping) {
		print z Mapping[z];
	}
	}
