// utilities

_.mixin({
  partition: function(coll, n) {
    return _.chain(coll)
      .groupBy(function(x, i) { return Math.floor(i/n); })
      .map(function(val, key) { return val; })
      .value();
  }
})

function fail(thing) {
  throw new Error(thing);
}

function warn(thing) {
  console.log(["WARNING:", thing].join(' '));
}

function note(thing) {
  console.log(["NOTE:", thing].join(' '));
}

function existy(x) { return x != null };

function truthy(x) { return (x !== false) && existy(x) };

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

function PrimitiveType(id, name, render) {
  this.id = id;
  this.name = name;
  this.render = render;
}

PrimitiveType.prototype.onClick = function(data) {
  return null;
}

vm_nil = new PrimitiveType(1, "nil", function(_) { return "nil"; });
vm_true = new PrimitiveType(2, "true",  function(_) { return "true"; })
vm_stack = new PrimitiveType(3, "stack", prefixed("st"))
vm_codeptr = new PrimitiveType(4, "codeptr", prefixedHex("c"))
vm_obj = new PrimitiveType(5, "obj", prefixed("o"))
vm_prop = new PrimitiveType(6, "prop", prefixed("pid"))
vm_int = new PrimitiveType(7, "int", function(v) { return v; })
vm_sstring = new PrimitiveType(8, "sstring", prefixedHex("'"))
vm_dstring = new PrimitiveType(9, "dstring", prefixedHex("\""))
vm_list = new PrimitiveType(10, "list", prefixedHex("[]"))
vm_codeofs = new PrimitiveType(11, "codeofs", prefixed("of"))
vm_funcptr = new PrimitiveType(12, "funcptr", prefixedHex("()"))
vm_empty = new PrimitiveType(13, "empty", function(_) { return "empty"; })
vm_native = new PrimitiveType(14, "native-code", prefixedHex("n"))
vm_enum = new PrimitiveType(15, "enum", prefixed("e"))
vm_bifptr = new PrimitiveType(16, "bifptr", prefixedHex("bf"))
vm_objx = new PrimitiveType(17, "objx", prefixed("ox"))

var types = [vm_nil, vm_true, vm_stack, vm_codeptr, vm_obj, vm_prop,
             vm_int, vm_sstring, vm_dstring, vm_list, vm_codeofs, vm_funcptr,
             vm_empty, vm_native, vm_enum, vm_bifptr, vm_objx];

function type(o) { return _.find(types, function(t) { return existy(t) && t.id == o.type; })}
function value(o) { return o.value; }

var cH = 20; // cell height
var cW = 70; // cell width
var cP = 3;  // cell padding
var rLabelWidth = 25;

/*
 * Maintain a group of cells under svg.
 * val(d) -> typed value from d
 * x(d, k), y(d, k) -> cell pos from d
 * key(d) -> key
 * prefix(d) -> prefix label
 */
function cells(svg, style, data, x, y, key, val, prefix) {

  if (!existy(val)) {
    val = _.identity;
  }

  var prefixPad = existy(prefix) ? rLabelWidth : 0;

  var rects = svg.selectAll("rect." + style).data(data, key);
  
  rects
    .enter()
    .append("rect");

  rects
    .on("click", function(d) { vm.updateToContext(val(d)); })
    .transition()
    .attr(
      cAttrs({
        x: function(d, k) { return x(d, k) * (cW + cP + prefixPad) + prefixPad; },
        y: function(d, k) { return y(d, k) * (cH + cP); },
        class: function(d) { return "vm-" + type(val(d)).name + " " + style; }
      }));

  rects
    .exit()
    .remove();

  var labels = svg.selectAll("text." + style + "-label").data(data, key);

  labels
    .enter()
    .append("text");
  
  labels
    .text(function(d) { return type(val(d)).render(value(val(d))); })
    .on("click", function(d) { return vm.updateToContext(val(d)); })
    .transition()
    .attr({
      x: function(d, k) { return x(d, k) * (cW + cP + prefixPad) + cW / 2 + prefixPad; },
      y: function (d, k) { return (cH + cP) * y(d, k) + 15},
      class: function(d) { return "vm-" + type(val(d)).name + " " + style + "-label"; }
    });
  
  labels
    .exit()
    .remove();

  if (existy(prefix)) {
    
    var prefixes = svg.selectAll("text." + style + "-prefix").data(data, key);

    prefixes
      .enter()
      .append("text");

    prefixes
      .text(function(d) { return prefix(d); })
      .attr({
        x: function(d, k) { return x(d, k) * (cW + cP + prefixPad) + prefixPad / 2; },
        y: function (d, k) { return (cH + cP) * y(d, k) + 15; },
        class: style + "-prefix"
      });

    prefixes
      .exit()
      .remove();

  }
}

function cAttrs(obj) { 
  return _.extend({x: 0, 
                   y: 0, 
                   rx: 3, 
                   ry: 3, 
                   width: cW, 
                   height: cH
                  }, 
                  obj)
}


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
    .on("click", function(d) { d3.xhr(d.href).post().on("load", function() { vm.update(); }); return false; })
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

// Information Panel

function InformationPanel(div) {
  this.div = div;
  this.meta = this.div.append("div").attr("class", "meta");
  this.meta.append("h1").text("VM:");
  this.fnsd = this.div.append("div").attr("class", "fnsd");
  this.fnsd.append("h1").text("Function Sets:");
  this.mcld = this.div.append("div").attr("class", "mcld");
  this.mcld.append("h1").text("Metaclasses:");
  this.symd = this.div.append("div").attr("class", "symd");
  this.symd.append("h1").text("Symbols:");
}

InformationPanel.prototype.update = function(vm) {
  note("updating info panel");

  var details = this.meta
    .selectAll("span.meta")
    .data(_.pairs(_.pick(vm, "id", "sequence")));

  details
    .enter()
    .append("span");

  details
    .attr("class", "meta")
    .text(function(d) { return d[0] + ": " + d[1]; });

  details
    .exit()
    .remove();

  var fns = this.fnsd
    .selectAll("span.builtin")
    .data(vm.fnsd);

  fns
    .enter()
    .append("span");

  fns
    .attr("class", "builtin")
    .text(function(d, i) { return "" + i + ": " + d; });
  
  fns.exit().remove();

  var mcs = this.mcld
    .selectAll("span.metaclass")
    .data(vm.mcld);

  mcs
    .enter()
    .append("span");

  mcs
    .attr("class", "metaclass")
    .text(function(d, i) { return "" + i + ": " + d["metaclass-id"]; });

  mcs.exit().remove();

  var syms = this.symd
    .selectAll("span.symbol")
    .data(vm.symd);

  syms
    .enter()
    .append("span");

  syms
    .attr("class", "symbol")
    .text(function(d) { return d.name; });

  syms.exit().remove();
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

  cells(this.svg, 
        "stack-item", 
        stack, 
        function() { return 0; },
        function(d, i) { return stack.length - i - 1; });
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
  cells(this.svg,
        "register",
        registers,
        function(d, i) { return i; },
        function() { return 0; },
        null,
        function(d) { return d.value; },
        function(d) { return d.name; });
}

// Object Diagram
function ObjectPoolDiagram(div) {
  this.div = div;
  this.svg = this.div
    .append("svg")
    .attr("class", "object");
}

/**
 * Update display to show the specified section of the object pool, 
 * using the metaclass lookup list provided.
 */
ObjectPoolDiagram.prototype.update = function(objectSection, mcld) {

  var key = function(d) { return d.oid.value; }
  var index = function(d) { return _.indexOf(objectSection, d); };

  // -- first the oid cells

  cells(this.svg,
        "oid",
        objectSection,
        function() { return 0; },
        index,
        key,
        function(d) { return d.oid; });

  // -- then the mc cells
  var mcs = 
    this.svg
      .selectAll("rect.metaclass")
      .data(objectSection, key);

  mcs
    .attr("y", function(d) { return (cH + cP) * index(d); })
    .enter()
    .append("rect")
    .attr(
      cAttrs(
        {
          y: function(d) { return (cH + cP) * index(d)},
          x: cW + cP,
          class: "metaclass"
        }));

  mcs.exit()
    .remove();

  var mclabels = 
    this.svg
      .selectAll("text.metaclass")
      .data(objectSection, key);

  mclabels
    .attr("y", function(d) { return (cH + cP) * index(d) + 15; })
    .enter()
    .append("text")
    .text(function(d) { return truncateMetaclassName(mcld[d.value.metaclass]['metaclass-id']); })
    .attr({
      x: cW + cP + cW / 2,
      y: function (d) { return (cH + cP) * index(d) + 15},
      class: "metaclass"
    });

  mclabels.exit()
    .remove();
}

function ObjectInspectorDiagram(div) {
  this.div = div;
  this.svg = this.div
    .append("svg")
    .attr("class", "inspector");
}

ObjectInspectorDiagram.prototype.update = function(object) {

  // object header
  cells(this.svg,
        "object",
        [object],
        function() { return 0; },
        function() { return 0; },
        function(d) { return d.oid; },
        function(d) { return d.oid; });

  // bases
  var baseObjects = 
    _.chain(object.value.bases)
     .map(function (value) { return {type: vm_obj.id, value: value}; })
    .value();

  cells(this.svg,
        "base",
        baseObjects,
        function() { return 0; },
        function(d, i) { return 1 + i; });

  // properties

  var countBases = _.has(object.value, "bases") ? object.value.bases.length : 0;
  var properties =
    _.chain(object.value.properties)
    .map(function(value, pid) { return {pid: {type: vm_prop.id, value:  parseInt(pid)}, value: value}; })
    .map(function(value, index) { return _.extend(value, {index: index}); })
    .value();

  function key (data) { return [object.oid.value, data.pid.value]; }
  function index (data) { return data.index; }

  cells(this.svg,
        "property",
        properties,
        function() { return 0; },
        function(d) { return 1 + countBases + index(d); },
        key,
        function(d) { return d.pid; });
  
  // property values
  cells(this.svg,
        "property-value",
        properties,
        function() { return 1; },
        function(d) { return 1 + countBases + index(d); },
        key,
        function(d) { return d.value; });
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
        _.extend(vm, o);
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
        _.extend(vm, o);
        vm.updateStatus();
        vm.updateStack();
        vm.updateRegisters();
        vm.updateConst(0, 512);
        vm.updateObjects(0, 40);
        infoPanel.update(vm);
      });
    }
  },

  updateMetaclassList: function() {
    var vm = this;
    if (vm._links) {
      d3.json(vm._links.mcld.href, function(m) {
        _.extend(vm, m);
        infoPanel.update(vm);
      });
    }
  },

  updateFunctionList: function() {
    var vm = this;
    if (vm._links) {
      d3.json(vm._links.fnsd.href, function(f) {
        _.extend(vm, f);
        infoPanel.update(vm);
      });
    }
  },

  updateSymbols: function() {
    var vm = this;
    if (vm._links) {
      d3.json(vm._links.symd.href, function(f) {
        _.extend(vm, f);
        infoPanel.update(vm);
      });
    }
  },

  updateStack: function() {
    var vm = this;
    if (vm._links) {
      d3.json(vm._links.stack.href, function(s) {
        _.extend(vm, s);
        stackDiagram.update(vm.stack);
      });
    }
  },

  updateRegisters: function() {
    var vm = this;
    if (vm._links) {
      d3.json(vm._links.registers.href, function(r) {
        _.extend(vm, r);
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
        _.extend(vm, cs);
        vm.codeSection = cs['code-section'];
        codeDiagram.update(vm.codeSection);
      });
    }
  },

  updateConst: function(address, length) {
    var vm = this;
    if (vm._links) {
      d3.json(detemplatiseUrl(vm._links.const.href) + '?address=' + address + '&length=' + length, function(cs) {
        _.extend(vm, cs);
        vm.constSection = cs['const-section'];
        constDiagram.update(vm.constSection);
      });
    }
  },

  updateObjects: function (oid, count) {
    var vm = this;
    if (vm._links) {
      d3.json(detemplatiseUrl(vm._links.objects.href) + '?oid=' + oid + '&count=' + count, function(os) {
        _.extend(vm, os);
        vm.objectSection = os['objs'];
        objectPoolDiagram.update(vm.objectSection, vm.mcld);
        objectInspectorDiagram.update(vm.objectSection[0]);
      });
    }
  },

  updateStatus: function(exc) {
    var vm = this;
    if (vm._links) {
      d3.json(vm._links.exc.href, function(e) {
        _.extend(vm, e);
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


  updateToContext: function(typedValue) {
    console.log(typedValue);
    switch (typedValue.type) {
      case vm_codeptr.id:
      case vm_funcptr.id:
      this.updateCode(typedValue.value, 512);
      break;
      case vm_obj.id:
      case vm_objx.id:
      this.updateObjects(typedValue.value, 40);
      break;
      case vm_list.id:
      case vm_sstring.id:
      case vm_dstring.id:
      this.updateConst(typedValue.value, 512);
      break;
    }
  }
}

var actionPanel;
var stackDiagram;
var registerDiagram;
var objectPoolDiagram;
var objectInspectorDiagram;
var codeDiagram;
var constDiagram;
var infoPanel;

function init() {

  actionPanel = new ControlPanel(d3.select("div#controls"));
  infoPanel = new InformationPanel(d3.select("div#information"));
  stackDiagram = new StackDiagram(d3.select("div.stack"));
  registerDiagram = new RegisterDiagram(d3.select("div.register"));
  objectPoolDiagram = new ObjectPoolDiagram(d3.select("div.object"));
  codeDiagram = new PoolDiagram(d3.select("div.code"));
  constDiagram = new PoolDiagram(d3.select("div.constant"));
  objectInspectorDiagram = new ObjectInspectorDiagram(d3.select("div.inspector"));

  vm.init();
}

init();

