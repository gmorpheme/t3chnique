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
    .on("click", function(d) { return vm.updateToContext(d); })
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
    .on("click", function(d) { return vm.updateToContext(d); })
    .text(function(d) { return types[d.type].render(d.value) })
    .attr({
      x: cW / 2,
      y: function (d, i) { return (cH + cP) * (stack.length - i - 1) + 15},
      class: function(d) { return "vm-" + types[d.type].name; }
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
    .attr("class", function(d) { return "vm-" + types[d.value.type].name; })
    .enter()
    .append("rect")
    .on("click", function(d) { return vm.updateToContext(d.value); })
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
      y: 15
    })
    .text(function (d) { return d.name });

  this.svg.selectAll("text.value")
    .data(registers)
    .text(function(d) { return types[d.value.type].render(d.value.value) })
    .enter()
    .append("text")
    .on("click", function(d) { return vm.updateToContext(d.value); })
    .attr({
      class: function(d) { return "value vm-" + types[d.value.type].name; },
      x: function (d, i) { return rLabelWidth + (cW / 2) + (cW + rLabelWidth + cP) * i },
      y: 15
    })
    .text(function(d) { return types[d.value.type].render(d.value.value) });
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

  var cells = 
    this.svg
      .selectAll("rect.oid")
      .data(objectSection, key);

  cells
    .attr("y", function(d, k) { return (cH + cP) * index(d); })
    .enter()
    .append("rect")
    .on("click", function(d) { return vm.updateToContext(d.oid); })
    .attr(
      cAttrs(
        {
          y: function(d, k) { return (cH + cP) * index(d) },
          class: function(d) { return "oid vm-" + types[d.oid.type].name; }
        }));

  cells.exit().remove();

  var labels = 
    this.svg
      .selectAll("text.oid")
      .data(objectSection, key);
  
  labels
    .attr("y", function(d, k) { return (cH + cP) * index(d) + 15; })
    .enter()
    .append("text")
    .text(function(d) { return types[d.oid.type].render(d.oid.value) })
    .on("click", function(d) { return vm.updateToContext(d.oid); })
    .attr({
      x: cW / 2,
      y: function (d, k) { return (cH + cP) * index(d) + 15},
      class: function(d) { return "vm-" + types[d.oid.type].name + " oid"; }
    });

  labels
    .exit().remove();

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
  var countBases = _.has(object.value, "bases") ? object.value.bases.length : 0;
  var countProps = _.has(object.value, "properties") ? object.value.bases.length : 0;
  
  var properties =
    _.chain(object.value.properties)
    .map(function(value, pid) { return {pid: {type: vm_prop.id, value:  parseInt(pid)}, value: value}; })
    .map(function(value, index) { return _.extend(value, {index: index}); })
    .value();

  var baseObjects = 
    _.chain(object.value.bases)
     .map(function (value) { return {type: vm_obj.id, value: value}; })
    .value();

  function key (data) { return [object.oid.value, data.pid.value]; }
  function index (data) { return data.index; }

  console.log("Properties: ", properties);
  console.log("Bases: ", baseObjects);

  // object header
  var header =
    this.svg
    .selectAll("rect.object")
    .data([object]);
  
  header
    .enter()
    .append("rect")
    .attr(cAttrs({
      class: "object vm-obj"
    }));

  header
    .on("click", function(d) { return vm.updateToContext(d.oid); })

  header.exit().remove();

  var headerlabel = 
    this.svg
    .selectAll("text.object")
    .data([object]);

  headerlabel
    .enter()
    .append("text")
    .attr(cAttrs({
      x: cW / 2, 
      y: 15,
      class: "object vm-obj"
    }));

  headerlabel
    .text(function(d) { return types[d.oid.type].render(d.oid.value); })
    .on("click", function(d) { return vm.updateToContext(d.oid); });

  headerlabel.exit().remove();

  // bases
  var bases = 
    this.svg
    .selectAll("rect.base")
    .data(baseObjects);

  bases
    .enter()
    .append("rect")
    .attr(
      cAttrs({
        class: function(d) { return "base vm-" + types[d.type].name; }
      }));

  bases
    .attr("y", function(d, i) { return (cH + cP) * (1 + i); })
    .on("click", function(d) { return vm.updateToContext(d); });

  bases.exit().remove();

  var baselabels =
    this.svg
    .selectAll("text.base")
    .data(baseObjects);

  baselabels
    .enter()
    .append("text")
    .attr({
      x: cW / 2,
      class: function(d) { return "vm-" + types[d.type].name + " base"; }
    });

  baselabels
    .on("click", function(d) { return vm.updateToContext(d); })
    .text(function(d) { return types[d.type].render(d.value); })
    .attr("y", function (d, i) { return (cH + cP) * (1 + i) + 15});

  baselabels.exit().remove();

  var props = 
    this.svg
    .selectAll("rect.prop")
    .data(properties, key);

  props
    .enter()
    .append("rect")
    .on("click", function(d) { return vm.updateToContext(d.pid); })
    .attr(
      cAttrs(
        {
          class: function(d) { return "prop vm-" + types[d.pid.type].name; }
        }));

  props.attr("y", function(d) { return (cH + cP) * (1 + countBases + index(d)); });
    
  props
    .exit().remove();

  var proplabels =
    this.svg
    .selectAll("text.prop")
    .data(properties, key);

  proplabels
    .enter()
    .append("text")
    .on("click", function(d) { return vm.updateToContext(d.pid); })
    .text(function(d) { return types[d.pid.type].render(d.pid.value) })
    .attr({
      x: cW / 2,
      class: function(d) { return "vm-" + types[d.pid.type].name + " prop"; }
    });

  proplabels.attr("y", function (d, k) { return (cH + cP) * (1 + countBases + index(d)) + 15});

  proplabels
    .exit().remove();
  
  var propvals = 
    this.svg
    .selectAll("rect.propval")
    .data(properties, key);

  propvals
    .enter()
    .append("rect")
    .on("click", function(d) { return vm.updateToContext(d.value); })
    .attr(
      cAttrs(
        {
          x: cW + cP,
          class: function(d) { return "propval vm-" + types[d.value.type].name; }
        }));

  propvals.attr("y", function(d) { return (cH + cP) * (1 + countBases + index(d)); });

  propvals.exit().remove();

  var propvallabels =
    this.svg
    .selectAll("text.propval")
    .data(properties, key);

  propvallabels
    .enter()
    .append("text")
    .on("click", function(d) { return vm.updateToContext(d.value); })
    .text(function(d) { return types[d.value.type].render(d.value.value) })
    .attr({
      x: cW + cP + cW / 2,
      class: function(d) { return "vm-" + types[d.pid.type].name + " propval"; }
    });

  propvallabels.attr("y", function (d, k) { return (cH + cP) * (1 + countBases + index(d)) + 15});

  propvallabels.exit().remove();

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
var objectPoolDiagram;
var objectInspectorDiagram;
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
        objectPoolDiagram.update(vm.objectSection, vm.mcld);
        objectInspectorDiagram.update(vm.objectSection[0]);
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

function init() {

  actionPanel = new ControlPanel(d3.select("div#controls"));
  stackDiagram = new StackDiagram(d3.select("div.stack"));
  registerDiagram = new RegisterDiagram(d3.select("div.register"));
  objectPoolDiagram = new ObjectPoolDiagram(d3.select("div.object"));
  codeDiagram = new PoolDiagram(d3.select("div.code"));
  constDiagram = new PoolDiagram(d3.select("div.constant"));
  objectInspectorDiagram = new ObjectInspectorDiagram(d3.select("div.inspector"));

  vm.init();
}

init();

