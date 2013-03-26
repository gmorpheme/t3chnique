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



function fakeSection() {
  var bytes = [];
  var addr  = 16 * _.random(0, 1024);
  for (var i = 0; i < 512; ++i) {
    bytes.push(_.random(0,255));
  }
  return {address: addr, bytes: bytes};
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


/*
 * Test data.
 */
var vm = {
  stack: [],
  registers: [],
  codeSection: {},
  constSection: {},
  objectSection: {},

  /**
   * Refresh stack from server.
   */
  refreshStack: function() {
    var depth = _.random(10, 50);
    for (var i = 0; i < depth; ++i) {
      vm.stack.push([_.random(1, 17), _.random(0, 10000)]);
    }
  },

  refreshRegisters: function() {
    vm.registers = [
      {name: "r0", value: [_.random(1, 17), _.random(0, 10000)]},
      {name: "ip", value: [4, _.random(0, 10000)]},
      {name: "ep", value: [4, _.random(0, 10000)]},
      {name: "sp", value: [3, _.random(0, vm.stack.length)]},
      {name: "fp", value: [3, _.random(0, vm.stack.length)]},
    ];
  },

  refreshCodeSection: function() {
    vm.codeSection = fakeSection();
  },

  refreshConstSection: function() {
    vm.constSection = fakeSection();
  },

  refreshObjectSection: function() {
    vm.objectSection = fakeObjectPool();
  }
}

/**
 * Turn a code or const section into a list of rows with address headers.
 */
function toTable(section) {
  return _.chain(section.bytes)
    .partition(16)
    .map(function (bytes, i) { return {address: section.address + i * 16, bytes: bytes } })
    .value();
}

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
  vm.refreshStack();
  vm.refreshRegisters();
  vm.refreshCodeSection();
  vm.refreshConstSection();
  vm.refreshObjectSection();

  var stacksvg = 
    d3.select("div.stack")
    .append("svg")
    .attr("class", "stack")
    .attr("width", w)
    .attr("height", (cellHeight + cellPadding) * vm.stack.length)
    .attr("fill", dgrey);

  // initialise registers svg
  svg = d3.select("div.register")
    .append("svg")
    .attr("class", "register")
    .attr("height", cellHeight + 2 * cellPadding);

  svg.selectAll("rect")
    .data(vm.registers)
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
    .data(vm.registers)
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
    .data(vm.registers)
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

  initPoolDiv(d3.select("div.code"), vm.codeSection);
  initPoolDiv(d3.select("div.constant"), vm.constSection);
  initObjectDiv(d3.select("div.object"), vm.objectSection);
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

  var cells = svg.selectAll("rect").data(vm.objectSection);

  cells
    .attr({
      y: function(d, i) { return (cellHeight + cellPadding) * (vm.objectSection.length - i - 1)},
      fill: function(d) { return types[d.oid[0]].fill }
    })
    .enter()
    .append("rect")
    .attr({
      x: 0,
      y: function(d, i) { return (cellHeight + cellPadding) * (vm.objectSection.length - i - 1)},
      rx: 3,
      ry: 3,
      width: cellWidth,
      height: cellHeight,
      fill: function(d) { return types[d.oid[0]].fill }
    });

  cells.exit().remove();

  var labels = svg.selectAll("text").data(vm.objectSection);

  labels
    .text(function(d) { return types[d.oid[0]].render(d.oid[1]) })
    .attr({
      y: function (d, i) { return (cellHeight + cellPadding) * (vm.objectSection.length - i - 1) + 15},
      fill: function (d, i) { return types[d.oid[0]].text }
    });
  
  labels
    .enter()
    .append("text")
    .text(function(d) { return types[d.oid[0]].render(d.oid[1]) })
    .attr({
      x: cellWidth / 2,
      y: function (d, i) { return (cellHeight + cellPadding) * (vm.objectSection.length - i - 1) + 15},
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
      .attr("height", (cellHeight + cellPadding) * vm.stack.length);

  var cells =   stacksvg.selectAll("rect").data(vm.stack);
  cells
    .attr({
      y: function(d, i) { return (cellHeight + cellPadding) * (vm.stack.length - i - 1)},
      fill: function(d) { return types[d[0]].fill }
    })
    .enter()
    .append("rect")
    .attr({
      x: 0,
      y: function(d, i) { return (cellHeight + cellPadding) * (vm.stack.length - i - 1)},
      rx: 3,
      ry: 3,
      width: cellWidth,
      height: cellHeight,
      fill: function(d) { return types[d[0]].fill }
    });

  cells.exit().remove();

  var labels = stacksvg.selectAll("text").data(vm.stack);

  labels
    .text(function(d) { return types[d[0]].render(d[1]) })
    .attr({
      y: function (d, i) { return (cellHeight + cellPadding) * (vm.stack.length - i - 1) + 15},
      fill: function (d, i) { return types[d[0]].text }
    });
  
  labels
    .enter()
    .append("text")
    .text(function(d) { return types[d[0]].render(d[1]) })
    .attr({
      x: cellWidth / 2,
      y: function (d, i) { return (cellHeight + cellPadding) * (vm.stack.length - i - 1) + 15},
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
  vm.stack.push([_.random(1, 17), _.random(0, 10000)]);
  updateStack();
})
d3.select("#pop").on("click", function() { 
  vm.stack.pop();
  updateStack();
})
d3.select("#ret").on("click", function() {
  vm.refreshRegisters();
  updateRegisters();
})


function updateRegisters() {
  var svg = d3.select("svg.register");

  svg.selectAll("rect")
    .data(vm.registers)
    .attr("fill", function(d) { return types[d.value[0]].fill } );

  svg.selectAll("text.value")
    .data(vm.registers)
    .attr("fill", function(d) { return types[d.value[0]].text })
    .text(function(d) { return types[d.value[0]].render(d.value[1]) });
}

init();
updateStack();
updateRegisters();

