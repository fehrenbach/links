var Regex = (function() {
function l(o) { return o.label; }
function v(o) { return o.value; }

function assert(n, msg) {
  if (!n) {
    throw ("Assertion failure! " + msg)
  }
}

function repeat(input) {
  switch (l(input)) {
    case 'Question' : return '?';
    case 'Star'     : return '*';
    case 'Plus'     : return '+';
    default         : assert(false, 'repeat : ' + l(input));
  }
}

function group(input) {
  return '(:?' + input + ')'
}

var specials = ['.','*','+','?','|','(',')','[',']','{','}','\\','$','^'];

var quoteRE = new RegExp(
  '(\\' + specials.join('|\\') + ')', 'g'
);

function quote(text) {
  return text.replace(quoteRE, '\\$1');
}

function compile(input) {
  var val = v(input);
  switch (l(input)) {
     case 'Range'  : return '[' + quote(val[1])  + '-' + quote(val[2]) + ']';
     case 'Simply' : return group(quote(val));
     case 'Any'    : return '.';
     case 'Seq'    : {
        var output = '';
        for (var i = 0; i < val.length; i++) {
          output += compile(val[i]);
        }
        return group(output);
     }
     case 'Repeat' : return group(compile(val[2]) + repeat(val[1]));
     default         : assert(false, 'compile : ' + input);
  }
}

function Range(l,r) { return {label:'Range', value:{1:l,2:r}}; }
function Simply(value) { return {label:'Simply', value:value}; }
var Any = { label:'Any', value:{}};
function Seq(value) { return {label:'Seq', value:value}; }
function Repeat(l,r) { return {label:'Repeat', value:{1:l,2:r}}; }
var Question = { label:'Question', value:{}};
var Plus = { label:'Plus', value:{}};
var Star = { label:'Star', value:{}};

var tests = 
  [
    (function(s) {
      return ["splicing", Simply (s), s, true];
    }("some .*string$\" ++?")),
    
    ["range 0", Range ('0', '9'), "3", true],
    ["range 1", Range ('0', '9'), "0", true],
    ["range 2", Range ('0', '9'), "9", true],
    ["range 3", Range ('0', '9'), ".", false],
    ["range 4", Range ('a', 'z'), "p", true],
    ["range 5", Range ('A', 'Z'), "p", false],

    ["star 0", Repeat (Star, Any), "23r2r3", true],
    ["star 1", Repeat (Star, Any), "", true],
    ["star 2", Repeat (Star, (Simply ("abc"))), "abc", true],
    ["star 3", Repeat (Star, (Simply ("abc"))), "abcabc", true],
    ["star 4", Repeat (Star, (Simply ("abc"))), "", true],
    ["star 5", Repeat (Star, (Simply ("abc"))), "a", false],
    ["star 6", Repeat (Star, (Simply ("abc"))), "abca", false],

    ["plus 0", Repeat (Plus, Any), "23r2r3", true],
    ["plus 1", Repeat (Plus, Any), "", false],
    ["plus 2", Repeat (Plus, (Simply ("abc"))), "abc", true],
    ["plus 3", Repeat (Plus, (Simply ("abc"))), "abcabc", true],
    ["plus 4", Repeat (Plus, (Simply ("abc"))), "", false],
    ["plus 5", Repeat (Plus, (Simply ("abc"))), "a", false],
    ["plus 6", Repeat (Plus, (Simply ("abc"))), "abca", false],

    ["nesting 0", Seq ([Simply ("A"),
                        Repeat (Plus, Simply ("B"))]), "ABBB", true],

    ["nesting 1", Seq ([Simply ("A"),
                        Repeat (Plus, Simply ("B"))]), "ABAB", false],

    ["nesting 2", Repeat (Plus, Seq ([Simply ("A"),
                                      Simply ("B")])), "ABAB", true],

    ["nesting 3", Repeat (Plus, Seq ([Simply ("A"),
                                      Simply ("B")])), "ABBB", false],
  ]

function run_tests(tests) {
  for (var i = 0; i < tests.length; i++) {
    var test = tests[i];
    var n = test[0], r = new RegExp('^' + compile(test[1]) + '$'), s = test[2], b = test[3];
    if ((r(s) != null) == b) {
      print("PASS: " + n);
    }
    else {
      print("FAIL: " + n);
    }
  }
}
return ({ 'compile' : compile });
})();