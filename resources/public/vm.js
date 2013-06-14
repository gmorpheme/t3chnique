// utilities

_.mixin({
  partition: function(coll, n) {
    return _.chain(coll)
      .groupBy(function(x, i) { return Math.floor(i/n); })
      .map(function(val, key) { return val; })
      .value();
  }
})

/**
 * Abbreviate a register name to two characters.
 */
function abbreviateRegisterName(name) {
  return name.length > 2 ? "".concat.apply("", _.map(name.split('-'), _.first)) : name;
}

function truncateMetaclassName(name) {
  return "".concat.apply("",_.map(name.split('-'), _.first));
}

var byteFormat = d3.format("02x");
var addrFormat = d3.format("#08x");
function prefixed(prefix) { return function(val) { return prefix + ":" + val} }
function prefixedHex(prefix) { return function(val) { return prefix + ":" + addrFormat(val); }} 

/**
 * Turn a code or const section into a list of rows with address headers.
 */
function toTable(section) {
  return _.chain(section.bytes)
    .partition(16)
    .map(function (bytes, i) { return {address: section.address + i * 16, bytes: bytes } })
    .value();
}

function PrimitiveType(name, render) {
  this.name = name;
  this.render = render;
}

PrimitiveType.prototype.onClick = function(data) {
  return null;
}

vm_nil = new PrimitiveType("nil", function(_) { return "nil"; });
vm_true = new PrimitiveType("true",  function(_) { return "true"; })
vm_stack = new PrimitiveType("stack", prefixed("st"))
vm_codeptr = new PrimitiveType("codeptr", prefixedHex("c"))
vm_obj = new PrimitiveType("obj", prefixed("o"))
vm_prop = new PrimitiveType("prop", prefixed("pid"))
vm_int = new PrimitiveType("int", function(v) { return v; })
vm_sstring = new PrimitiveType("sstring", prefixedHex("'"))
vm_dstring = new PrimitiveType("dstring", prefixedHex("\""))
vm_list = new PrimitiveType("list", prefixedHex("[]"))
vm_codeofs = new PrimitiveType("codeofs", prefixed("of"))
vm_funcptr = new PrimitiveType("funcptr", prefixedHex("()"))
vm_empty = new PrimitiveType("empty", function(_) { return "empty"; })
vm_native = new PrimitiveType("native-code", prefixedHex("n"))
vm_enum = new PrimitiveType("enum", prefixed("e"))
vm_bifptr = new PrimitiveType("bifptr", prefixedHex("bf"))
vm_objx = new PrimitiveType("objx", prefixed("ox"))

var types = [null,
             vm_nil,
             vm_true,
             vm_stack,
             vm_codeptr,
             vm_obj,
             vm_prop,
             vm_int,
             vm_sstring,
             vm_dstring,
             vm_list,
             vm_codeofs,
             vm_funcptr,
             vm_empty,
             vm_native,
             vm_enum,
             vm_bifptr,
             vm_objx];

var cH = 20; // cell height
var cW = 70; // cell width
var cP = 3;  // cell padding
var rLabelWidth = 25;
function cAttrs(obj) { return _.extend({x: 0, y: 0, rx: 3, ry: 3, width: cW, height: cH}, obj)}


// Actions
function ControlPanel(div) {
  this.div = div;
}

/**
 * Expect each link to be a {href, name} object.
 */
ControlPanel.prototype.update = function(links, exc) {

  var actions = this.div
    .selectAll("a.action")
    .data(links, function(d) { return d.href; });
  
  actions
    .enter()
    .append("span")
    .attr("id", function(d) { return d.rel.replace('/', '-'); })
    .attr("class", "action")
    .append("a")
    .attr("href", "#")
    .attr("class", "action")
    .on("click", function(d) { console.log("Starting post"); d3.xhr(d.href).post().on("load", function() { console.log("handling post response"); vm.update(); }); return false; })
    .text(function (d) { return d.name; });

  actions
    .exit()
    .remove();
  
  var exc = 
    this.div
    .selectAll("a.exc")
    .data(exc ? [exc] : []);

  exc.enter()
    .append("span")
    .attr("class", "exc")
    .append("a")
    .attr("href", "")
    .attr("class", "exc")
    .text(function (d) { return d.substr(0, d.indexOf(":"));});

  exc.exit().remove();

  enrichActions(); 
}

function formatOpCode(selector, assembly) {
  var op = d3.select(selector);
  var a = op.select('a');

  a.text('');

  a.append('span')
    .attr('class', 'prompt')
    .text('>');

  a.append('span')
    .attr('class', 'mnemonic')
    .text(assembly.op.mnemonic);

  a.selectAll('span.op-arg')
    .data(_.pairs(assembly.args))
    .enter()
    .append('span')
    .attr('class', 'op-arg')
    .text(function(d) { return d[0] + ':' + d[1]; });

  op.selectAll('a.additional')
    .data(_.pairs(assembly._links))
    .enter()
    .append('a')
    .attr('class', 'additional')
    .attr('target', '_blank')
    .attr('href', function(d) { return d[1].href; })
    .text(function(d) { return '(' + d[0] + ')'; });
}

function enrichActions() {

  if (vm.registers) {
    // use disassembly as link for step
    var dis1link = vm._links.dis1.href;
    dis1link = dis1link.substr(0, dis1link.indexOf('{'));

    var ip = _.find(vm.registers, function(r) { return r.name === 'ip'; });
    if (ip) {
      dis1link = dis1link + ip.value.value;

      d3.json(dis1link, function(o) { vm.assembly = o.assembly; formatOpCode('#action-step', vm.assembly)})
    }
  }
}

// Stack Diagram
function StackDiagram(div) {
  this.div = div;
  this.svg = 
    this.div
    .append("svg")
    .attr("class", "stack");
};

StackDiagram.prototype.update = function(stack) {
  this.svg
    .attr("height", (cH + cP) * _.max([10, stack.length]));

  var cells = this.svg.selectAll("rect").data(stack);

  cells
    .attr({
      y: function(d, i) { return (cH + cP) * (stack.length - i - 1)},
      class: function(d) { return "vm-" + types[d.type].name; }
    })
    .enter()
    .append("rect")
    .attr(
      cAttrs(
        {
          y: function(d, i) { return (cH + cP) * (stack.length - i - 1)},
          class: function(d) { return "vm-" + types[d.type].name; }
        }));

  cells.exit().remove();

  var labels = this.svg.selectAll("text").data(stack);

  labels
    .text(function(d) { return types[d.type].render(d.value) })
    .attr({
      y: function (d, i) { return (cH + cP) * (stack.length - i - 1) + 15},
      class: function(d) { return "vm-" + types[d.type].name; }
    });
  
  labels
    .enter()
    .append("text")
    .text(function(d) { return types[d.type].render(d.value) })
    .attr({
      x: cW / 2,
      y: function (d, i) { return (cH + cP) * (stack.length - i - 1) + 15},
      class: function(d) { return "vm-" + types[d.type].name; },
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
    .attr("height", cH + 2 * cP);
}

RegisterDiagram.prototype.update = function(registers) {
  this.svg.selectAll("rect")
    .data(registers)
    .enter()
    .append("rect")
    .attr(
      cAttrs(
        {
          x: function (d, i) { return rLabelWidth + (cW + rLabelWidth + cP) * i },
          class: function(d) { return "vm-" + types[d.value.type].name; }
        }));

  this.svg.selectAll("text.label")
    .data(registers)
    .enter()
    .append("text")
    .attr({
      class: function(d) { return "label vm-" + types[d.value.type].name; },
      x: function(d, i) { return (rLabelWidth / 2) + (cW + rLabelWidth + cP) * i; },
      y: 15,
      "text-anchor": "middle",
      "font-family": "sans-serif",
      "font-size": "13px",
    })
    .text(function (d) { return d.name });

  this.svg.selectAll("text.value")
    .data(registers)
    .text(function(d) { return types[d.value.type].render(d.value.value) })
    .enter()
    .append("text")
    .attr({
      class: function(d) { return "value vm-" + types[d.value.type].name; },
      x: function (d, i) { return rLabelWidth + (cW / 2) + (cW + rLabelWidth + cP) * i },
      y: 15,
      "text-anchor": "middle",
      "font-family": "sans-serif",
      "font-size": "13px",
      "font-weight": "bold",
    })
    .text(function(d) { return types[d.value.type].render(d.value.value) });
}

// Object Diagram
function ObjectDiagram(div) {
  this.div = div;
  this.svg = this.div
    .append("svg")
    .attr("class", "object");
}

/**
 * Update display to show the specified section of the object pool, 
 * using the metaclass lookup list provided.
 */
ObjectDiagram.prototype.update = function(objectSection, mcld) {

  var key = function(d) { return d.oid.value; }
  var index = function(k) { return k - objectSection[0].oid.value + 1; };

  // -- first the oid cells

  var cells = 
    this.svg
      .selectAll("rect.oid")
      .data(objectSection, key);

  cells
    .enter()
    .append("rect")
    .attr(
      cAttrs(
        {
          y: function(d, k) { return (cH + cP) * index(k)},
          class: function(d) { return "vm-" + types[d.oid.type].name + " oid"; }
        }));

  cells.exit().remove();

  var labels = 
    this.svg
      .selectAll("text.oid")
      .data(objectSection, key);

  labels
    .text(function(d) { return types[d.oid.type].render(d.oid.value) })
    .attr({
      y: function (d, k) { return (cH + cP) * index(k) + 15},
      class: function(d) { return "vm-" + types[d.oid.type].name + " oid"; }
    });
  
  labels
    .enter()
    .append("text")
    .text(function(d) { return types[d.oid.type].render(d.oid.value) })
    .attr({
      x: cW / 2,
      y: function (d, k) { return (cH + cP) * index(k) + 15},
      "text-anchor": "middle",
      "font-family": "sans-serif",
      "font-size": "13px",
      "font-weight": "bold"
    });

  labels
    .exit().remove();

  // -- then the mc cells
  var mcs = 
    this.svg
      .selectAll("rect.mc")
      .data(objectSection, key);

  mcs.enter()
    .append("rect")
    .attr(
      cAttrs(
        {
          y: function(d, k) { return (cH + cP) * index(k)},
          x: cW + cP,
          class: "metaclass"
        }));

  mcs.exit()
    .remove();

  var mclabels = 
    this.svg
      .selectAll("text.mc")
      .data(objectSection, key);

  mclabels.enter()
    .append("text")
    .text(function(d) { return truncateMetaclassName(mcld[d.value.metaclass]['metaclass-id']); })
    .attr({
      x: cW + cP + cW / 2,
      y: function (d, k) { return (cH + cP) * index(k) + 15},
      "text-anchor": "middle",
      "font-family": "sans-serif",
      "font-weight": "bold",
      class: "metaclass"
    });

  mclabels.exit()
    .remove();

  var pgroups = 
    this.svg
      .selectAll("g.props")
      .data(objectSection, key);

  pgroups
    .enter()
    .append("g")
    .attr("class", "props");

  pgroups
    .exit()
    .remove();

  var props = pgroups
    .selectAll("rect.fieldprop")
    .data(function (d, k) { return _.map(d.value.properties, function(value, pid) { return {pid: pid, value: value, index: index(k)}; }); });

  props
    .enter()
    .append("rect")
    .attr(
      cAttrs(
        {x: function(d, i) { return (cW + cP) * (2 + (i * 2));  },
         y: function (d) { return (cH + cP) * d.index; },
         class: "vm-prop fieldprop"
        }
      )
    );

  props
    .exit()
    .remove();

  var proplabels = pgroups
    .selectAll("text.fieldprop")
    .data(function (d, k) { return _.map(d.value.properties, function(value, pid) { return {pid: pid, value: value, index: index(k)}; }); });

  proplabels
    .enter()
    .append("text")
    .text(function(d) { return vm_prop.render(d.pid); })
    .attr({
      x: function(d, i) { return (cW + cP) * (2.5 + (i * 2));  },
      y: function (d, k) { return (cH + cP) * d.index + 15},
      "text-anchor": "middle",
      "font-family": "sans-serif",
      "font-weight": "bold",
      class: function(d) { return "vm-pid fieldprop"; }}
    );

  proplabels
    .exit()
    .remove();

  var vals = pgroups
    .selectAll("rect.fieldval")
    .data(function (d, k) { return _.map(d.value.properties, function(value, pid) { return {pid: pid, value: value, index: index(k)}; }); });

  vals
    .enter()
    .append("rect")
    .attr(
      cAttrs(
        {x: function(d, i) { return (cW + cP) * (3 + (i * 2));  },
         y: function (d) { return (cH + cP) * d.index; },
         class: function(d) { return "vm-" + types[d.value.type].name + " fieldval"; }
        }
      )
    );

  vals
    .exit()
    .remove();


  var vallabels = pgroups
    .selectAll("text.fieldval")
    .data(function (d, k) { return _.map(d.value.properties, function(value, pid) { return {pid: pid, value: value, index: index(k)}; }); });

  vallabels
    .enter()
    .append("text")
    .text(function(d) { return types[d.value.type].render(d.value.value); })
    .attr({
      x: function(d, i) { return (cW + cP) * (3.5 + (i * 2));  },
      y: function (d, k) { return (cH + cP) * d.index + 15},
      "text-anchor": "middle",
      "font-family": "sans-serif",
      "font-weight": "bold",
      class: function(d) { return "fieldval"; }}
    );

  vallabels
    .exit()
    .remove();
}

// Pool Diagram (code or const)
function PoolDiagram(div) {
  this.div = div;
  this.table = this.div.append("table");
}

PoolDiagram.prototype.update = function(section) {
  var table = toTable(section);

  var rows = 
    this.table.selectAll("tr")
    .data(table, function (d) { return d.address; });

  rows
    .enter()
    .append("tr")
    .html(function(d) { return "<th>" + addrFormat(d.address) + "</th>"; })
    .selectAll("td.byte")
    .data(function(d) { return d.bytes; })
    .enter()
    .append("td")
    .attr("class", "byte")
    .text(function (d) { return byteFormat(d); });

  rows
    .exit()
    .remove();

}

var actionPanel;
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
  mcld: [],
  fnsd: [],
  symd: [],
  codeSection: {},
  constSection: {},
  objectSection: {},

  init: function() {
    console.log("In init");
    var vm = this;
    if (vm_url) {
      d3.json(vm_url, function(o) {
        console.log("In init response");
        vm._links = o._links;
        vm.updateMetaclassList();
        vm.updateFunctionList();
        vm.updateSymbols();
        vm.update();
      });
    }
  },

  update: function() {
    console.log("In update");
    var vm = this;
    if (vm_url) {
      d3.json(vm_url, function(o) {
        console.log("Handling vm response");
        vm._links = o._links;
        vm.updateStatus();
        vm.updateStack();
        vm.updateRegisters();
        vm.updateConst(0, 512);
        vm.updateObjects(0, 40);
      });
    }
  },

  updateMetaclassList: function() {
    var vm = this;
    if (vm._links) {
      d3.json(vm._links.mcld.href, function(m) {
        vm._links = m._links;
        vm.mcld = m.mcld;
      });
    }
  },

  updateFunctionList: function() {
    var vm = this;
    if (vm._links) {
      d3.json(vm._links.fnsd.href, function(f) {
        vm._links = f._links;
        vm.fnsd = f.fnsd;
      });
    }
  },

  updateSymbols: function() {
    var vm = this;
    if (vm._links) {
      d3.json(vm._links.symd.href, function(f) {
        vm._links = f._links;
        vm.symd = f.symd;
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
        enrichActions();
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
        objectDiagram.update(vm.objectSection, vm.mcld);
      });
    }
  },

  updateStatus: function(exc) {
    var vm = this;
    if (vm._links) {
      d3.json(vm._links.exc.href, function(e) {
        vm._links = e._links;
        vm.exc = e.exc;
        actionPanel.update(_.filter(vm._links, function(lk, k) { lk.rel = k; return k.indexOf("action/") >= 0; }), vm.exc);
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
    var addr = Math.floor(this.getInstructionPointer() / 16) * 16;
    var len = 512;
    this.updateCode(addr, len);
  },
}

function init() {

  actionPanel = new ControlPanel(d3.select("div#controls"));
  stackDiagram = new StackDiagram(d3.select("div.stack"));
  registerDiagram = new RegisterDiagram(d3.select("div.register"));
  objectDiagram = new ObjectDiagram(d3.select("div.object"));
  codeDiagram = new PoolDiagram(d3.select("div.code"));
  constDiagram = new PoolDiagram(d3.select("div.constant"));

  vm.init();
}

init();

