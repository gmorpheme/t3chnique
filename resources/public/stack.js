var dgrey = "#202020";
var lgrey = "#808080";
var red1 = "#DE2740";
var red2 = "#A4404E";
var red3 = "#8D0C1E";
var red4 = "#EE5A6E";
var red5 = "#EE8190";
var teal1 = "#1A957D";
var teal2 = "#2B6E61";
var teal3 = "#085E4E";
var teal4 = "#4CC9B1";
var teal5 = "#6DC9B7";
var leaf1 = "#ACDC26";
var leaf2 = "#88A33F";
var leaf3 = "#6A8C0C";
var leaf4 = "#C6ED5A";
var leaf5 = "#D0ED80";

/*
 * Test data.
 */
function rnd(low, high) { return Math.floor(Math.random() * (high - low) + low); }
var stack = [];
var depth = rnd(10, 50);
for (var i = 0; i < depth; ++i) {
  stack.push([rnd(1, 18), rnd(0, 10000)]);
}

var registers =[ 
  {name: "r0", value: [7, 23]},
  {name: "ip", value: [3, 24]}
]

/*
 * Primitive type configuration.
 */

function typeRender(name, fill, text, render) {
  return {name: name, fill: fill, text: text, render: render}
}

function prefixed(prefix) { return function(val) { return prefix + ":" + val.toString()} }
function prefixedHex(prefix) { return function(val) { return prefix + ":0x" + val.toString(16)}} 

var types = [null,
             typeRender("nil", red1, "white", function(val) { return "nil" }),
             typeRender("true", leaf1, "white", function(val) { return "true" }),
             typeRender("stack", leaf1, "white", function(val) { return "st:" + val }),
             typeRender("codeptr", red2, "white", prefixedHex("c")),
             typeRender("obj", teal3, "white", prefixed("o")),
             typeRender("prop", red4, "white", function(val) { return "pid:" + val} ),
             typeRender("int", teal1, "white", function(val) { return val }),
             typeRender("sstring", leaf2, "white", prefixedHex("'")),
             typeRender("dstring", leaf3, "white", prefixedHex("\"")),
             typeRender("list", leaf2, "white", prefixedHex("[]")),
             typeRender("codeofs", red5, "white", prefixed("off")),
             typeRender("funcptr", red4, "white", prefixedHex("()")),
             typeRender("empty", red1, "white", function(val) {return "empty"}),
             typeRender("native-code", red4, "white", prefixedHex("n")),
             typeRender("enum", teal1, "white", prefixed("e")),
             typeRender("bifptr", red4, "white", prefixedHex("bif")),
             typeRender("objx", teal3, "white", prefixed("ox"))];

var cellHeight = 20;
var cellWidth = 70;
var cellPadding = 3;
var rLabelWidth = 15;
var w = 100;

function init() {
  var stacksvg = 
    d3.select("div.stack")
    .append("svg")
    .attr("class", "stack")
    .attr("width", w)
    .attr("height", (cellHeight + cellPadding) * stack.length)
    .attr("fill", dgrey);
}

function updateStack() {

  var stacksvg = 
    d3.select("svg.stack")
      .attr("height", (cellHeight + cellPadding) * stack.length);

  var cells =   stacksvg.selectAll("rect").data(stack);
  cells
    .attr({
      y: function(d, i) { return (cellHeight + cellPadding) * (stack.length - i - 1)},
      fill: function(d) { return types[d[0]].fill }
    })
    .enter()
    .append("rect")
    .attr({
      x: 0,
      y: function(d, i) { return (cellHeight + cellPadding) * (stack.length - i - 1)},
      rx: 3,
      ry: 3,
      width: cellWidth,
      height: cellHeight,
      fill: function(d) { return types[d[0]].fill }
    });

  cells.exit().remove();

  var labels = stacksvg.selectAll("text").data(stack);

  labels
    .text(function(d) { return types[d[0]].render(d[1]) })
    .attr({
      y: function (d, i) { return (cellHeight + cellPadding) * (stack.length - i - 1) + 15},
      fill: function (d, i) { return types[d[0]].text }
    });
  
  labels
    .enter()
    .append("text")
    .text(function(d) { return types[d[0]].render(d[1]) })
    .attr({
      x: cellWidth / 2,
      y: function (d, i) { return (cellHeight + cellPadding) * (stack.length - i - 1) + 15},
      fill: function (d, i) { return types[d[0]].text },
      "text-anchor": "middle",
      "font-family": "sans-serif",
      "font-size": "13px",
      "font-weight": "bold"
    });

  labels
    .exit().remove();
}

d3.select("#push").on("click", function() { 
  stack.push([rnd(1, 18), rnd(0, 10000)]);
  updateStack();
})
d3.select("#pop").on("click", function() { 
  stack.pop();
  updateStack();
})

function registerDiagram() {
  svg = d3.select("div.register")
    .append("svg")
    .attr("id", "reg");

  svg.selectAll("rect")
    .data(registers)
    .enter()
    .append("rect")
    .attr({
      x: function (d, i) { return rLabelWidth + (cellWidth + rLabelWidth + cellPadding) * i },
      y: 0,
      rx: 3,
      ry: 3,
      width: cellWidth,
      height: cellHeight,
      fill: function(d) { return types[d.value[0]].fill }
    });

  svg.selectAll("text.label")
    .data(registers)
    .enter()
    .append("text")
    .attr({
      class: "label",
      x: function(d, i) { return (cellWidth + rLabelWidth + cellPadding) * i; },
      y: 15,
      fill: lgrey,
      "font-family": "sans-serif",
      "font-size": "13px",
    })
    .text(function (d) { return d.name });

  svg.selectAll("text.value")
    .data(registers)
    .enter()
    .append("text")
    .attr({
      class: "value",
      x: function (d, i) { return rLabelWidth + (cellWidth / 2) + (cellWidth + rLabelWidth + cellPadding) * i },
      y: 15,
      "text-anchor": "middle",
      "font-family": "sans-serif",
      "font-size": "13px",
      "font-weight": "bold",
      fill: function(d) { return types[d.value[0]].text }
    })
    .text(function(d) { return types[d.value[0]].render(d.value[1]) });

}

init();
updateStack();
registerDiagram();

