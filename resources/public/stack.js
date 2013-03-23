/*
 * Test data.
 */
var stack = [ 
  [7, 9876],
  [11, 0x1633],
  [1, 0],
  [2, 0],
  [7, 1],
  [12, 0x88],
  [8, 0x788],
  [9, 0x728],
  [1, 0],
  [1, 0] ];

var registers = {
  r0: [7, 23],
  ip: 24
}

/*
 * Primitive type configuration.
 */

function typeRender(name, fill, border, text, render) {
  return {name: name, fill: fill, border: border, text: text, render: render}
}

function identity(val) { return val.toString() }
function hex(val) { return "0x" + val.toString(16) }

var types = [null,
             typeRender("nil", "white", "red", "red", function(val) { return "nil" }),
             typeRender("true", "rgb(00,80,20)", "white", "white", function(val) { return "true" }),
             typeRender("stack", "rgb(5,5,5)", "white", "white", function(val) { return "st:" + val }),
             typeRender("codeptr", "rgb(5,5,5)", "white", "white", hex ),
             typeRender("obj", "teal", "white", "white", hex),
             typeRender("prop", "rgb(10,30,80)", "white", "white", function(val) { return "pid:" + val} ),
             typeRender("int", "rgb(00,00,60)", "white", "white", identity),
             typeRender("sstring", "teal", "white", "white", hex),
             typeRender("dstring", "teal", "white", "white", hex),
             typeRender("list", "maroon", "white", "white", identity),
             typeRender("codeofs", "maroon", "white", "white", identity),
             typeRender("funcptr", "white", "blue", "blue", hex),
             typeRender("empty", "maroon", "white", "white", identity),
             typeRender("native-code", "maroon", "white", "white", identity),
             typeRender("enum", "maroon", "white", "white", identity),
             typeRender("bifptr", "maroon", "white", "white", identity),
             typeRender("objx", "maroon", "white", "white", identity)];


function stackDiagram() {

  w = 100;
  h = 500;
  cellHeight = 20;
  cellWidth = 70;
  cellPadding = 3;

  var stacksvg = 
    d3.select("body")
    .append("svg")
    .attr("id", "stack")
    .attr("width", w)
    .attr("height", h);

  stacksvg.selectAll("rect")
    .data(stack)
    .enter()
    .append("rect")
    .attr({
      x: 0,
      y: function(d, i) { return (cellHeight + cellPadding) * i},
      rx: 3,
      ry: 3,
      width: cellWidth,
      height: cellHeight,
      fill: function(d) { return types[d[0]].fill },
      "stroke-width": "1px",
      stroke: function(d) { return types[d[0]].border }
    });

  stacksvg.selectAll("text")
    .data(stack)
    .enter()
    .append("text")
    .text(function(d) { return types[d[0]].render(d[1]) })
    .attr({
      x: cellWidth / 2,
      y: function (d, i) { return (cellHeight + cellPadding) * i + 15},
      fill: function (d, i) { return types[d[0]].text },
      "text-anchor": "middle",
      "font-family": "sans-serif",
      "font-size": "13px"
    });

  return stacksvg;
}

function registerDiagram() {
  svg = d3.selectAll("body")
    .append("svg")
    .attr("id", "reg");

  svg.selectAll("rect")
    .data(registers)
    .enter();
}

function codePoolDiagram() {}

stackDiagram()
registerDiagram() 
codePoolDiagram()
