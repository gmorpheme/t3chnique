// utilities

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

function abbreviateRegisterName(name) {
  return name.length > 2 ? "".concat.apply("", _.map(name.split('-'), _.first)) : name;
}

var byteFormat = d3.format("02x");
var addrFormat = d3.format("#08x");

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
var rLabelWidth = 25;
var w = 100;

// Stack Diagram
function StackDiagram(div) {
  this.div = div;
  this.svg = 
    this.div
    .append("svg")
    .attr("class", "stack")
    .attr("width", w)
    .attr("fill", dgrey);
};

StackDiagram.prototype.update = function(stack) {
  this.svg
    .attr("height", (cellHeight + cellPadding) * _.max([10, stack.length]))
    .attr("width", w);

  var cells = this.svg.selectAll("rect").data(stack);

  cells
    .attr({
      y: function(d, i) { return (cellHeight + cellPadding) * (stack.length - i - 1)},
      fill: function(d) { return types[d.type].fill }
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
      fill: function(d) { return types[d.type].fill }
    });

  cells.exit().remove();

  var labels = this.svg.selectAll("text").data(stack);

  labels
    .text(function(d) { return types[d.type].render(d.value) })
    .attr({
      y: function (d, i) { return (cellHeight + cellPadding) * (stack.length - i - 1) + 15},
      fill: function (d, i) { return types[d.type].text }
    });
  
  labels
    .enter()
    .append("text")
    .text(function(d) { return types[d.type].render(d.value) })
    .attr({
      x: cellWidth / 2,
      y: function (d, i) { return (cellHeight + cellPadding) * (stack.length - i - 1) + 15},
      fill: function (d, i) { return types[d.type].text },
      "text-anchor": "middle",
      "font-family": "sans-serif",
      "font-size": "13px",
      "font-weight": "bold"
    });

  labels
    .exit().remove();
}

// Register Diagram
function RegisterDiagram(div) {
  this.div = div;
  this.svg = this.div
    .append("svg")
    .attr("class", "register")
    .attr("height", cellHeight + 2 * cellPadding);
}

RegisterDiagram.prototype.update = function(registers) {
  this.svg.selectAll("rect")
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
      fill: function(d) { return types[d.value.type].fill }
    });

  this.svg.selectAll("text.label")
    .data(registers)
    .enter()
    .append("text")
    .attr({
      class: "label",
      x: function(d, i) { return (rLabelWidth / 2) + (cellWidth + rLabelWidth + cellPadding) * i; },
      y: 15,
      fill: lgrey,
      "text-anchor": "middle",
      "font-family": "sans-serif",
      "font-size": "13px",
    })
    .text(function (d) { return d.name });

  this.svg.selectAll("text.value")
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
      fill: function(d) { return types[d.value.type].text }
    })
    .text(function(d) { return types[d.value.type].render(d.value.value) });
}

// Object Diagram
function ObjectDiagram(div) {
  this.div = div;
  this.svg = this.div
    .append("svg")
    .attr("class", "object")
    .attr("height", (cellHeight + cellPadding) * 100);
}

ObjectDiagram.prototype.update = function(objectSection) {
  var cells = this.svg.selectAll("rect").data(objectSection);

  cells
    .attr({
      y: function(d, i) { return (cellHeight + cellPadding) * (objectSection.length - i - 1)},
      fill: function(d) { return types[d.oid.type].fill }
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
      fill: function(d) { return types[d.oid.type].fill }
    });

  cells.exit().remove();

  var labels = this.svg.selectAll("text").data(objectSection);

  labels
    .text(function(d) { return types[d.oid.type].render(d.oid.value) })
    .attr({
      y: function (d, i) { return (cellHeight + cellPadding) * (objectSection.length - i - 1) + 15},
      fill: function (d, i) { return types[d.oid.type].text }
    });
  
  labels
    .enter()
    .append("text")
    .text(function(d) { return types[d.oid.type].render(d.oid.value) })
    .attr({
      x: cellWidth / 2,
      y: function (d, i) { return (cellHeight + cellPadding) * (objectSection.length - i - 1) + 15},
      fill: function (d, i) { return types[d.oid.type].text },
      "text-anchor": "middle",
      "font-family": "sans-serif",
      "font-size": "13px",
      "font-weight": "bold"
    });

  labels
    .exit().remove();
}

// Pool Diagram (code or const)
function PoolDiagram(div) {
  this.div = div;
  this.table = this.div.append("table");
}

PoolDiagram.prototype.update = function(section) {
  this.table.selectAll("tr")
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

var stackDiagram;
var registerDiagram;
var objectDiagram;
var codeDiagram;
var constDiagram;

function detemplatiseUrl(u) {
  return u.substr(0, u.indexOf('{'));
}

/*
 * The main VM representation object.
 */
var vm = {
  _links: null,

  stack: [],
  registers: [],
  codeSection: {},
  constSection: {},
  objectSection: {},

  update: function() {
    var vm = this;
    if (vm_url) {
      d3.json(vm_url, function(o) {
        vm._links = o._links;
        vm.updateStack();
        vm.updateRegisters();
        vm.updateConst(0, 512);
        vm.updateObjects(0, 30);
      });
    }
  },

  updateStack: function() {
    var vm = this;
    if (vm._links) {
      d3.json(vm._links.stack.href, function(s) {
        vm._links = s._links;
        vm.stack = s.stack;
        stackDiagram.update(vm.stack);
      });
    }
  },

  updateRegisters: function() {
    var vm = this;
    if (vm._links) {
      d3.json(vm._links.registers.href, function(r) {
        vm._links = r._links;
        vm.registers = 
          _.chain(r)
          .map(function(v, k) { return {name: abbreviateRegisterName(k), value: v}})
          .filter(function(o) { return o.name[0] !== '_'; })
          .value();
        registerDiagram.update(vm.registers);

        vm.updateCodeToContext();
      });
    }
  },

  updateCode: function(address, length) {
    var vm = this;
    if (vm._links) {
      d3.json(detemplatiseUrl(vm._links.code.href) + '?address=' + address + '&length=' + length, function(cs) {
        vm._links = cs._links;
        vm.codeSection = cs['code-section'];
        codeDiagram.update(vm.codeSection);
      });
    }
  },

  updateConst: function(address, length) {
    var vm = this;
    if (vm._links) {
      d3.json(detemplatiseUrl(vm._links.const.href) + '?address=' + address + '&length=' + length, function(cs) {
        vm._links = cs._links;
        vm.constSection = cs['const-section'];
        constDiagram.update(vm.constSection);
      });
    }
  },

  updateObjects: function (oid, count) {
    var vm = this;
    if (vm._links) {
      d3.json(detemplatiseUrl(vm._links.objects.href) + '?oid=' + oid + '&count=' + count, function(os) {
        vm._links = os._links;
        vm.objectSection = os['objs'];
        objectDiagram.update(vm.objectSection);
      });
    }
  },

  getInstructionPointer: function() {
    return _.find(this.registers, function(r) { return r.name === 'ip'; }).value.value;
  },
  
  getEntryPoint: function() {
    return _.find(this.registers, function(r) { return r.name === 'ep'; }).value.value;
  },

  updateCodeToContext: function() {
    var addr = Math.floor(this.getInstructionPointer() / 16);
    var len = 512;
    this.updateCode(addr, len);
  },
}

function init() {

  stackDiagram = new StackDiagram(d3.select("div.stack"));
  registerDiagram = new RegisterDiagram(d3.select("div.register"));
  objectDiagram = new ObjectDiagram(d3.select("div.object"));
  codeDiagram = new PoolDiagram(d3.select("div.code"));
  constDiagram = new PoolDiagram(d3.select("div.constant"));

  vm.update();
}


// bind fake actions to buttons for testing

d3.select("#push").on("click", function() { 
  vm.stack.push({type: _.random(1, 17), value: _.random(0, 10000)});
  stackDiagram.update(vm.stack);
});
d3.select("#pop").on("click", function() { 
  vm.stack.pop();
  stackDiagram.update(vm.stack);
});
d3.select("#ret").on("click", function() {
  vm.randomiseRegisters();
  updateRegisters();
});
d3.select("#codeseek").on("click", function() {
  vm.randomiseCodeSection();
  updateCodeSection();
});
d3.select("#constseek").on("click", function() {
  vm.randomiseConstSection();
  updateConstSection();
});
d3.select("#objseek").on("click", function() {
  vm.randomiseObjectSection();
  updateObjectSection();
});

init();
