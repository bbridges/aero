defmodule Aero.LexerTest do
  use Aero.LexerCase

  lexer_test "empty string",
    source: "",
    lines: 1,
    tokens: []

  lexer_test "whitespace is classified as newlines without spaces",
    source: "\r\n  t_1 t_2 \r t_3 \nt_4\r\nt_5 \n \n t_6 ",
    lines: 6,
    tokens: [
      {:newline, 1},
      {:ident, 2, :t_1},
      {:ident, 2, :t_2},
      {:ident, 2, :t_3},
      {:newline, 2},
      {:ident, 3, :t_4},
      {:newline, 3},
      {:ident, 4, :t_5},
      {:newline, 4},
      {:ident, 6, :t_6},
    ]

  lexer_test "semicolons act as newlines",
    source: "t_1;t_2 ; t_3\n;t_4 t_5;;",
    lines: 2,
    tokens: [
      {:ident, 1, :t_1},
      {:newline, 1},
      {:ident, 1, :t_2},
      {:newline, 1},
      {:ident, 1, :t_3},
      {:newline, 1},
      {:ident, 2, :t_4},
      {:ident, 2, :t_5},
      {:newline, 2}
    ]

  lexer_test "newlines are tokenized after comments",
    source: "-- comment\nt_1-- comment ",
    lines: 2,
    tokens: [
      {:newline, 1},
      {:ident, 2, :t_1}
    ]

  lexer_test "basic string",
    source: "\"test\"",
    lines: 1,
    tokens: [
      {:string_lit, 1, "test"}
    ]

  lexer_test "identifiers",
    source: "test Test __TEST__ test_1 Test1 __TEST_1__",
    lines: 1,
    tokens: [
      {:ident, 1, :test},
      {:ident, 1, :Test},
      {:ident, 1, :__TEST__},
      {:ident, 1, :test_1},
      {:ident, 1, :Test1},
      {:ident, 1, :__TEST_1__}
    ]

  lexer_test "basic atoms",
    source: ":test :test_1",
    lines: 1,
    tokens: [
      {:atom_lit, 1, :test},
      {:atom_lit, 1, :test_1}
    ]

  lexer_test "escaped atoms",
    source: ":\"Test\" :\"& ,#\"",
    lines: 1,
    tokens: [
      {:atom_lit, 1, :Test},
      {:atom_lit, 1, :"& ,#"}
    ]
end
