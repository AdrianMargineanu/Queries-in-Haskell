module Query where

import UserInfo
import Rating
import Movie

type Column = String
type TableSchema = [Column]
type Field = String
type Entry = [Field]

data Table = Table TableSchema [Entry]

type ColSeparator = Char
type LnSeparator = Char

get_Schema :: Table -> TableSchema
get_Schema (Table schema entry) = schema

get_Entry :: Table -> [Entry]
get_Entry (Table schema entry) = entry

sub_string_before :: Char -> String -> String -> String
sub_string_before _  [] return_s = return_s
sub_string_before char original_s return_s 
                        | (head original_s) == char = return_s
                        | otherwise = sub_string_before char (tail original_s) (return_s ++ [head original_s]) 

sub_string_after :: Char -> String -> String 
sub_string_after _ [] = []
sub_string_after char s
                     | (head s) == char = tail s
                     | otherwise = sub_string_after char (tail s)

-- TODO 1
read_table :: ColSeparator -> LnSeparator -> String -> Table
read_table col_sep ln_sep str = Table (read_line col_sep (sub_string_before ln_sep str [])) (read_entry col_sep ln_sep (sub_string_after ln_sep str))

read_entry :: ColSeparator -> LnSeparator -> String -> [Entry]
read_entry _ _ [] = []
read_entry col_sep ln_sep str = [(read_line col_sep (sub_string_before ln_sep str []))] ++ (read_entry col_sep ln_sep (sub_string_after ln_sep str)) 
                        

read_line :: ColSeparator -> String -> [String]
read_line _ [] = []
read_line col_sep s = [(sub_string_before col_sep s [])] ++ (read_line col_sep (sub_string_after col_sep s)) 

user_info = read_table '|' '\n' UserInfo.user_info_str
rating = read_table ' ' '\n' Rating.rating_str
movie = read_table '|' '\n' Movie.movie_str

-- TODO 2
max_vec :: [Int] -> [Int] -> [Int]
max_vec [] [] = []
max_vec l s 
            | (head l) > (head s) = (head l) : (max_vec (tail l) (tail s))
            | otherwise = (head s) : (max_vec (tail l) (tail s))

show_one_simbol :: Int -> Char -> String
show_one_simbol 0 _ = []
show_one_simbol n char = char:(show_one_simbol (n-1) char)

show_line :: [Int] -> [String] -> String
show_line _ [] = []
show_line l str = "|" ++ (head str) ++ (show_one_simbol (head l - (length (head str))) ' ') ++ (show_line (tail l) (tail str))

show_Entries :: [Int] -> [Entry]-> String
show_Entries _ [] = []
show_Entries l entry = (show_line l (head entry)) ++ "\n" ++ (show_Entries l (tail entry))

calc_max_col :: [Int] -> [Entry] -> [Int]
calc_max_col l [] = l
calc_max_col l entry = calc_max_col (max_vec l (map length (head entry))) (tail entry)

show_table :: [Int] -> Table -> String
show_table l (Table header entries) = (show_one_simbol (foldl (+) 0 l) '-') ++ "\n" ++ (show_line l header) ++ "\n" ++ (show_one_simbol (foldl (+) 0 l) '-') ++  "\n" ++ (show_Entries l entries) ++ (show_one_simbol (foldl (+) 0 l) '-')

instance Show Table where
    show (Table header entries) = show_table (calc_max_col (map length header) entries) (Table header entries)

data FilterCondition = Lt Field Integer | Eq Field String | In Field [String] | Not FilterCondition

-- TODO 3
smaller :: Field -> TableSchema -> Integer -> Entry -> Bool
smaller _ [] _ [] = False
smaller field schema n entry
                          | field == head schema = compare (head entry) n
                          |otherwise = smaller field (tail schema) n (tail entry)
                          where 
                            compare s n
                                     | n > (read s :: Integer) = True
                                     |otherwise = False

equal :: Field -> TableSchema -> String -> Entry -> Bool
equal _ [] _ [] = False
equal field schema n entry
                          | field == head schema = compare (head entry) n
                          |otherwise = equal field (tail schema) n (tail entry)
                          where 
                            compare s n
                                     | n == s = True
                                     |otherwise = False

in_range :: Field -> TableSchema -> [String] -> Entry -> Bool
in_range _ [] _ [] = False
in_range field schema n entry
                          | field == head schema = compare (head entry) n
                          |otherwise = in_range field (tail schema) n (tail entry)
                          where
                              compare [] _ = False
                              compare l s
                                      | (one_element (head l) (head s)) == True  = True
                                      | otherwise = compare (tail l) (tail s)
                                      where 
                                          one_element _ [] = False
                                          one_element elm s
                                                            | elm == head s = True
                                                            | otherwise = one_element elm (tail s)

getFilter :: FilterCondition -> TableSchema -> (Entry -> Bool)
getFilter (Lt field n) schema =  (\entry -> smaller field schema n entry)
getFilter (Eq field str) schema =  (\entry -> equal field schema str entry)
getFilter (In field str) schema =  (\entry -> in_range field schema str entry)
getFilter (Not filter) schema = (\entry -> not (getFilter filter schema entry))

-- TODO 4
data Query = Filter FilterCondition Query |  
             Select [String] Query |
             SelectLimit [String] Integer Query |
             Cosine Query |
             Query :|| Query |
             Atom Table

needed_col :: Int -> TableSchema -> [String] -> [Int]
needed_col _ _ [] = []
needed_col n schema str 
                    | (head schema) == (head str) = [n] ++ (needed_col (n+1) (tail schema) (tail str) )
                    | otherwise = needed_col (n+1) (tail schema) str

select :: [Int] -> [String] -> [String]
select [] _ = []
select l str = [(str !! (head l))] ++ (select (tail l) str)

limit_entry :: [Entry] -> Integer -> [Entry]
limit_entry _ 0 = []
limit_entry entry n = [(head entry)] ++ (limit_entry (tail entry) 0)

limit :: Table -> Integer -> Table
limit (Table schema entry) n = Table schema (limit_entry entry n)



eval :: Query -> Table
eval (Atom table) = table
eval (Filter fil_con q) = Table ( get_Schema (eval q)) ((\x -> (filter (getFilter fil_con (get_Schema x) ) (get_Entry x)  )) (eval q))
eval (Select l q) = Table ( select (needed_col 0 (get_Schema (eval q)) l) (get_Schema (eval q)) ) ( (\x -> map ( (select (needed_col 0 (get_Schema (eval q)) l ) ) ) x) (get_Entry (eval q)))
eval (SelectLimit l n q) = limit (eval (Select l q)) n
eval (q1 :|| q2) = Table ( get_Schema (eval q1) )  ((get_Entry (eval q1) ) ++ (get_Entry (eval q2) ))


-- TODO 5
same_zone :: String -> Query
same_zone str = Select ["user_id","occupation"] (Filter (Eq "zone" (last (last (take (read str) (get_Entry (user_info) ) ) ) ) ) (Atom user_info) )  

male_within_age :: Integer -> Integer -> Query
male_within_age n m = Select ["occupation","zone"] (Filter (Eq "sex" "M") (( Filter (Lt "age" n) (Atom user_info)) :|| ( Filter (Not( Lt "age" m)) (Atom user_info) ) ) )

mixed :: [String] -> [String] -> Integer -> Query
mixed zone occupation n = Select ["user_id"] (Filter (Lt "age" n) (Filter (In "zone" zone) (Filter (In "occupation" occupation) (Atom user_info) ) ) )