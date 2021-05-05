use "hw3.sml";

val sl = ["Abc", "y", "Z", "slug", "test"];
val only_caps = ["A", "Bee"]
val empty_list = [];
val singleton = ["one"]
val only_lowercase_1 = only_lowercase(sl) = ["y", "slug", "test"];
val only_lowercase_2 = only_lowercase(empty_list) = [];
val only_lowercase_3 = only_lowercase(only_caps) = [];
val longest_string1_1 = longest_string1(sl) = "slug";
val longest_string1_2 = longest_string1(only_caps) = "Bee";
val longest_string1_3 = longest_string1(empty_list) = "";
val longest_string1_4 = longest_string1(singleton) = "slug";
val longest_string2_1 = longest_string2(sl) = "test";
val longest_string2_2 = longest_string2(only_caps) = "Bee";
val longest_string2_3 = longest_string2(empty_list) = "";