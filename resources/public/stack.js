_.mixin({
  partition: function(coll, n) {
    return _.chain(coll)
      .groupBy(function(x, i) { return Math.floor(i/n); })
      .map(function(val, key) { return val; })
      .value();
  }
})

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
var stack = [];
var depth = _.random(10, 50);
for (var i = 0; i < depth; ++i) {
  stack.push([_.random(1, 17), _.random(0, 10000)]);
}

var registers = [ 
  {name: "r0", value: [7, 23]},
  {name: "ip", value: [4, 24]},
  {name: "ep", value: [4, 14]},
  {name: "sp", value: [3, 12]},
  {name: "fp", value: [3, 14]},
]

function fakeSection() {
  var bytes = [];
  var addr  = 16 * _.random(0, 1024);
  for (var i = 0; i < 512; ++i) {
    bytes.push(_.random(0,255));
  }
  return {address: addr, bytes: bytes};
}

var codeSection = fakeSection()
var constSection = fakeSection()

/**
 * Turn a codeSection object into a list of rows with address headers.
 */
function toTable(codeSection) {
  return _.chain(codeSection.bytes)
    .partition(16)
    .map(function (bytes, i) { return {address: codeSection.address + i * 16, bytes: bytes } })
    .value();
}

function fakeObjectPool() {
  var objects = [];
  var id = _.random(0, 20000);
  for (var i = 0; i < 100; ++i) {
    id += _.random(0, 24);
    objects.push({oid: [5, id]});
  }
  return objects;
}

var objectSection = fakeObjectPool();

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
  // initialise stack svg
  var stacksvg = 
    d3.select("div.stack")
    .append("svg")
    .attr("class", "stack")
    .attr("width", w)
    .attr("height", (cellHeight + cellPadding) * stack.length)
    .attr("fill", dgrey);

  // initialise registers svg
  svg = d3.select("div.register")
    .append("svg")
    .attr("class", "register")
    .attr("height", cellHeight + 2 * cellPadding);

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

  initPoolDiv(d3.select("div.code"), codeSection);
  initPoolDiv(d3.select("div.constant"), constSection);
  initObjectDiv(d3.select("div.object"), objectSection);
}

var byteFormat = d3.format("02x");
var addrFormat = d3.format("#08x");

function initObjectDiv(div) {
  var svg = div.append("svg")
    .attr("class", "object")
    .attr("height", (cellHeight + cellPadding) * 100);

  updateObjects();
}

function updateObjects() {
  var svg = 
    d3.select("svg.object");

  var cells = svg.selectAll("rect").data(objectSection);

  cells
    .attr({
      y: function(d, i) { return (cellHeight + cellPadding) * (objectSection.length - i - 1)},
      fill: function(d) { return types[d.oid[0]].fill }
    })
    .enter()
    .append("rect")
    .attr({
      x: 0,
      y: function(d, i) { return (cellHeight + cellPadding) * (objectSection.length - i - 1)},
      rx: 3,
      ry: 3,
      width: cellWidth,
      height: cellHeight,
      fill: function(d) { return types[d.oid[0]].fill }
    });

  cells.exit().remove();

  var labels = svg.selectAll("text").data(objectSection);

  labels
    .text(function(d) { return types[d.oid[0]].render(d.oid[1]) })
    .attr({
      y: function (d, i) { return (cellHeight + cellPadding) * (objectSection.length - i - 1) + 15},
      fill: function (d, i) { return types[d.oid[0]].text }
    });
  
  labels
    .enter()
    .append("text")
    .text(function(d) { return types[d.oid[0]].render(d.oid[1]) })
    .attr({
      x: cellWidth / 2,
      y: function (d, i) { return (cellHeight + cellPadding) * (objectSection.length - i - 1) + 15},
      fill: function (d, i) { return types[d.oid[0]].text },
      "text-anchor": "middle",
      "font-family": "sans-serif",
      "font-size": "13px",
      "font-weight": "bold"
    });

  labels
    .exit().remove();
}

function initPoolDiv(div, section) {
  var table = div.append("table");

  table.selectAll("tr")
    .data(toTable(section))
    .enter()
    .append("tr")
    .html(function(d) { return "<th>" + addrFormat(d.address) + "</th>"; })
    .selectAll("td.byte")
    .data(function(d) { return d.bytes; })
    .enter()
    .append("td")
    .attr("class", "byte")
    .text(function (d) { return byteFormat(d); })
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
  stack.push([_.random(1, 17), _.random(0, 10000)]);
  updateStack();
})
d3.select("#pop").on("click", function() { 
  stack.pop();
  updateStack();
})
d3.select("#ret").on("click", function() {
  registers = [
    {name: "r0", value: [_.random(1, 17), _.random(0, 10000)]},
    {name: "ip", value: [4, _.random(0, 10000)]}
  ];
  updateRegisters();
})


function updateRegisters() {
  var svg = d3.select("svg.register");

  svg.selectAll("rect")
    .data(registers)
    .attr("fill", function(d) { return types[d.value[0]].fill } );

  svg.selectAll("text.value")
    .data(registers)
    .attr("fill", function(d) { return types[d.value[0]].text })
    .text(function(d) { return types[d.value[0]].render(d.value[1]) });
}

init();
updateStack();
updateRegisters();

