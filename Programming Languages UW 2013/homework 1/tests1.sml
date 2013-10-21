is_older ((5, 4, 4), (4, 5,4) ) = false;
is_older ((1, 2, 25), (3, 5, 22) );
is_older ((1, 2, 25), (3, 5, 26) ) = true;
is_older ((3, 5, 26),  (1, 2, 25)  ) = true;
is_older ((1, 2, 25), (4, 5, 22) );


month_range (334, 365);

oldest ([(1,2,3),(5,2,3),(7,2,3),(3,2,3)]);

date_to_string (0, 1, 1);
date_to_string (1, 2, 25);
date_to_string (3,2,1);

number_before_reaching_sum (1, [2]);
number_before_reaching_sum (5, [3, 1, 2]);

number_before_reaching_sum (6, [4,1,1,1]);
number_before_reaching_sum (10, [1,2,3,4,5]);

oldest ([(5,5,2),(5,10,2),(5,2,2),(5,12,2)]);
oldest [(5,5,2), (5,2,2) ];
