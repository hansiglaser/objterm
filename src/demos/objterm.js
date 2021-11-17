var pas = {};

var rtl = {

  version: 20006,

  quiet: false,
  debug_load_units: false,
  debug_rtti: false,

  $res : {},

  debug: function(){
    if (rtl.quiet || !console || !console.log) return;
    console.log(arguments);
  },

  error: function(s){
    rtl.debug('Error: ',s);
    throw s;
  },

  warn: function(s){
    rtl.debug('Warn: ',s);
  },

  checkVersion: function(v){
    if (rtl.version != v) throw "expected rtl version "+v+", but found "+rtl.version;
  },

  hiInt: Math.pow(2,53),

  hasString: function(s){
    return rtl.isString(s) && (s.length>0);
  },

  isArray: function(a) {
    return Array.isArray(a);
  },

  isFunction: function(f){
    return typeof(f)==="function";
  },

  isModule: function(m){
    return rtl.isObject(m) && rtl.hasString(m.$name) && (pas[m.$name]===m);
  },

  isImplementation: function(m){
    return rtl.isObject(m) && rtl.isModule(m.$module) && (m.$module.$impl===m);
  },

  isNumber: function(n){
    return typeof(n)==="number";
  },

  isObject: function(o){
    var s=typeof(o);
    return (typeof(o)==="object") && (o!=null);
  },

  isString: function(s){
    return typeof(s)==="string";
  },

  getNumber: function(n){
    return typeof(n)==="number"?n:NaN;
  },

  getChar: function(c){
    return ((typeof(c)==="string") && (c.length===1)) ? c : "";
  },

  getObject: function(o){
    return ((typeof(o)==="object") || (typeof(o)==='function')) ? o : null;
  },

  isTRecord: function(type){
    return (rtl.isObject(type) && type.hasOwnProperty('$new') && (typeof(type.$new)==='function'));
  },

  isPasClass: function(type){
    return (rtl.isObject(type) && type.hasOwnProperty('$classname') && rtl.isObject(type.$module));
  },

  isPasClassInstance: function(type){
    return (rtl.isObject(type) && rtl.isPasClass(type.$class));
  },

  hexStr: function(n,digits){
    return ("000000000000000"+n.toString(16).toUpperCase()).slice(-digits);
  },

  m_loading: 0,
  m_loading_intf: 1,
  m_intf_loaded: 2,
  m_loading_impl: 3, // loading all used unit
  m_initializing: 4, // running initialization
  m_initialized: 5,

  module: function(module_name, intfuseslist, intfcode, impluseslist){
    if (rtl.debug_load_units) rtl.debug('rtl.module name="'+module_name+'" intfuses='+intfuseslist+' impluses='+impluseslist);
    if (!rtl.hasString(module_name)) rtl.error('invalid module name "'+module_name+'"');
    if (!rtl.isArray(intfuseslist)) rtl.error('invalid interface useslist of "'+module_name+'"');
    if (!rtl.isFunction(intfcode)) rtl.error('invalid interface code of "'+module_name+'"');
    if (!(impluseslist==undefined) && !rtl.isArray(impluseslist)) rtl.error('invalid implementation useslist of "'+module_name+'"');

    if (pas[module_name])
      rtl.error('module "'+module_name+'" is already registered');

    var r = Object.create(rtl.tSectionRTTI);
    var module = r.$module = pas[module_name] = {
      $name: module_name,
      $intfuseslist: intfuseslist,
      $impluseslist: impluseslist,
      $state: rtl.m_loading,
      $intfcode: intfcode,
      $implcode: null,
      $impl: null,
      $rtti: r
    };
    if (impluseslist) module.$impl = {
          $module: module,
          $rtti: r
        };
  },

  exitcode: 0,

  run: function(module_name){
    try {
      if (!rtl.hasString(module_name)) module_name='program';
      if (rtl.debug_load_units) rtl.debug('rtl.run module="'+module_name+'"');
      rtl.initRTTI();
      var module = pas[module_name];
      if (!module) rtl.error('rtl.run module "'+module_name+'" missing');
      rtl.loadintf(module);
      rtl.loadimpl(module);
      if (module_name=='program'){
        if (rtl.debug_load_units) rtl.debug('running $main');
        var r = pas.program.$main();
        if (rtl.isNumber(r)) rtl.exitcode = r;
      }
    } catch(re) {
      if (!rtl.showUncaughtExceptions) {
        throw re
      } else {  
        if (!rtl.handleUncaughtException(re)) {
          rtl.showException(re);
          rtl.exitcode = 216;
        }  
      }
    } 
    return rtl.exitcode;
  },
  
  showException : function (re) {
    var errMsg = rtl.hasString(re.$classname) ? re.$classname : '';
    errMsg +=  ((errMsg) ? ': ' : '') + (re.hasOwnProperty('fMessage') ? re.fMessage : re);
    alert('Uncaught Exception : '+errMsg);
  },

  handleUncaughtException: function (e) {
    if (rtl.onUncaughtException) {
      try {
        rtl.onUncaughtException(e);
        return true;
      } catch (ee) {
        return false; 
      }
    } else {
      return false;
    }
  },

  loadintf: function(module){
    if (module.$state>rtl.m_loading_intf) return; // already finished
    if (rtl.debug_load_units) rtl.debug('loadintf: "'+module.$name+'"');
    if (module.$state===rtl.m_loading_intf)
      rtl.error('unit cycle detected "'+module.$name+'"');
    module.$state=rtl.m_loading_intf;
    // load interfaces of interface useslist
    rtl.loaduseslist(module,module.$intfuseslist,rtl.loadintf);
    // run interface
    if (rtl.debug_load_units) rtl.debug('loadintf: run intf of "'+module.$name+'"');
    module.$intfcode(module.$intfuseslist);
    // success
    module.$state=rtl.m_intf_loaded;
    // Note: units only used in implementations are not yet loaded (not even their interfaces)
  },

  loaduseslist: function(module,useslist,f){
    if (useslist==undefined) return;
    var len = useslist.length;
    for (var i = 0; i<len; i++) {
      var unitname=useslist[i];
      if (rtl.debug_load_units) rtl.debug('loaduseslist of "'+module.$name+'" uses="'+unitname+'"');
      if (pas[unitname]==undefined)
        rtl.error('module "'+module.$name+'" misses "'+unitname+'"');
      f(pas[unitname]);
    }
  },

  loadimpl: function(module){
    if (module.$state>=rtl.m_loading_impl) return; // already processing
    if (module.$state<rtl.m_intf_loaded) rtl.error('loadimpl: interface not loaded of "'+module.$name+'"');
    if (rtl.debug_load_units) rtl.debug('loadimpl: load uses of "'+module.$name+'"');
    module.$state=rtl.m_loading_impl;
    // load interfaces of implementation useslist
    rtl.loaduseslist(module,module.$impluseslist,rtl.loadintf);
    // load implementation of interfaces useslist
    rtl.loaduseslist(module,module.$intfuseslist,rtl.loadimpl);
    // load implementation of implementation useslist
    rtl.loaduseslist(module,module.$impluseslist,rtl.loadimpl);
    // Note: At this point all interfaces used by this unit are loaded. If
    //   there are implementation uses cycles some used units might not yet be
    //   initialized. This is by design.
    // run implementation
    if (rtl.debug_load_units) rtl.debug('loadimpl: run impl of "'+module.$name+'"');
    if (rtl.isFunction(module.$implcode)) module.$implcode(module.$impluseslist);
    // run initialization
    if (rtl.debug_load_units) rtl.debug('loadimpl: run init of "'+module.$name+'"');
    module.$state=rtl.m_initializing;
    if (rtl.isFunction(module.$init)) module.$init();
    // unit initialized
    module.$state=rtl.m_initialized;
  },

  createCallback: function(scope, fn){
    var cb;
    if (typeof(fn)==='string'){
      cb = function(){
        return scope[fn].apply(scope,arguments);
      };
    } else {
      cb = function(){
        return fn.apply(scope,arguments);
      };
    };
    cb.scope = scope;
    cb.fn = fn;
    return cb;
  },

  createSafeCallback: function(scope, fn){
    var cb = function(){
      try{
        if (typeof(fn)==='string'){
          return scope[fn].apply(scope,arguments);
        } else {
          return fn.apply(scope,arguments);
        };
      } catch (err) {
        if (!rtl.handleUncaughtException(err)) throw err;
      }
    };
    cb.scope = scope;
    cb.fn = fn;
    return cb;
  },

  cloneCallback: function(cb){
    return rtl.createCallback(cb.scope,cb.fn);
  },

  eqCallback: function(a,b){
    // can be a function or a function wrapper
    if (a==b){
      return true;
    } else {
      return (a!=null) && (b!=null) && (a.fn) && (a.scope===b.scope) && (a.fn==b.fn);
    }
  },

  initStruct: function(c,parent,name){
    if ((parent.$module) && (parent.$module.$impl===parent)) parent=parent.$module;
    c.$parent = parent;
    if (rtl.isModule(parent)){
      c.$module = parent;
      c.$name = name;
    } else {
      c.$module = parent.$module;
      c.$name = parent.$name+'.'+name;
    };
    return parent;
  },

  initClass: function(c,parent,name,initfn,rttiname){
    parent[name] = c;
    c.$class = c; // Note: o.$class === Object.getPrototypeOf(o)
    c.$classname = rttiname?rttiname:name;
    parent = rtl.initStruct(c,parent,name);
    c.$fullname = parent.$name+'.'+name;
    // rtti
    if (rtl.debug_rtti) rtl.debug('initClass '+c.$fullname);
    var t = c.$module.$rtti.$Class(c.$classname,{ "class": c });
    c.$rtti = t;
    if (rtl.isObject(c.$ancestor)) t.ancestor = c.$ancestor.$rtti;
    if (!t.ancestor) t.ancestor = null;
    // init members
    initfn.call(c);
  },

  createClass: function(parent,name,ancestor,initfn,rttiname){
    // create a normal class,
    // ancestor must be null or a normal class,
    // the root ancestor can be an external class
    var c = null;
    if (ancestor != null){
      c = Object.create(ancestor);
      c.$ancestor = ancestor;
      // Note:
      // if root is an "object" then c.$ancestor === Object.getPrototypeOf(c)
      // if root is a "function" then c.$ancestor === c.__proto__, Object.getPrototypeOf(c) returns the root
    } else {
      c = { $ancestor: null };
      c.$create = function(fn,args){
        if (args == undefined) args = [];
        var o = Object.create(this);
        o.$init();
        try{
          if (typeof(fn)==="string"){
            o[fn].apply(o,args);
          } else {
            fn.apply(o,args);
          };
          o.AfterConstruction();
        } catch($e){
          // do not call BeforeDestruction
          if (o.Destroy) o.Destroy();
          o.$final();
          throw $e;
        }
        return o;
      };
      c.$destroy = function(fnname){
        this.BeforeDestruction();
        if (this[fnname]) this[fnname]();
        this.$final();
      };
    };
    rtl.initClass(c,parent,name,initfn,rttiname);
  },

  createClassExt: function(parent,name,ancestor,newinstancefnname,initfn,rttiname){
    // Create a class using an external ancestor.
    // If newinstancefnname is given, use that function to create the new object.
    // If exist call BeforeDestruction and AfterConstruction.
    var isFunc = rtl.isFunction(ancestor);
    var c = null;
    if (isFunc){
      // create pascal class descendent from JS function
      c = Object.create(ancestor.prototype);
      c.$ancestorfunc = ancestor;
      c.$ancestor = null; // no pascal ancestor
    } else if (ancestor.$func){
      // create pascal class descendent from a pascal class descendent of a JS function
      isFunc = true;
      c = Object.create(ancestor);
      c.$ancestor = ancestor;
    } else {
      c = Object.create(ancestor);
      c.$ancestor = null; // no pascal ancestor
    }
    c.$create = function(fn,args){
      if (args == undefined) args = [];
      var o = null;
      if (newinstancefnname.length>0){
        o = this[newinstancefnname](fn,args);
      } else if(isFunc) {
        o = new this.$func(args);
      } else {
        o = Object.create(c);
      }
      if (o.$init) o.$init();
      try{
        if (typeof(fn)==="string"){
          this[fn].apply(o,args);
        } else {
          fn.apply(o,args);
        };
        if (o.AfterConstruction) o.AfterConstruction();
      } catch($e){
        // do not call BeforeDestruction
        if (o.Destroy) o.Destroy();
        if (o.$final) o.$final();
        throw $e;
      }
      return o;
    };
    c.$destroy = function(fnname){
      if (this.BeforeDestruction) this.BeforeDestruction();
      if (this[fnname]) this[fnname]();
      if (this.$final) this.$final();
    };
    rtl.initClass(c,parent,name,initfn,rttiname);
    if (isFunc){
      function f(){}
      f.prototype = c;
      c.$func = f;
    }
  },

  createHelper: function(parent,name,ancestor,initfn,rttiname){
    // create a helper,
    // ancestor must be null or a helper,
    var c = null;
    if (ancestor != null){
      c = Object.create(ancestor);
      c.$ancestor = ancestor;
      // c.$ancestor === Object.getPrototypeOf(c)
    } else {
      c = { $ancestor: null };
    };
    parent[name] = c;
    c.$class = c; // Note: o.$class === Object.getPrototypeOf(o)
    c.$classname = rttiname?rttiname:name;
    parent = rtl.initStruct(c,parent,name);
    c.$fullname = parent.$name+'.'+name;
    // rtti
    var t = c.$module.$rtti.$Helper(c.$classname,{ "helper": c });
    c.$rtti = t;
    if (rtl.isObject(ancestor)) t.ancestor = ancestor.$rtti;
    if (!t.ancestor) t.ancestor = null;
    // init members
    initfn.call(c);
  },

  tObjectDestroy: "Destroy",

  free: function(obj,name){
    if (obj[name]==null) return null;
    obj[name].$destroy(rtl.tObjectDestroy);
    obj[name]=null;
  },

  freeLoc: function(obj){
    if (obj==null) return null;
    obj.$destroy(rtl.tObjectDestroy);
    return null;
  },

  hideProp: function(o,p,v){
    Object.defineProperty(o,p, {
      enumerable: false,
      configurable: true,
      writable: true
    });
    if(arguments.length>2){ o[p]=v; }
  },

  recNewT: function(parent,name,initfn,full){
    // create new record type
    var t = {};
    if (parent) parent[name] = t;
    var h = rtl.hideProp;
    if (full){
      rtl.initStruct(t,parent,name);
      t.$record = t;
      h(t,'$record');
      h(t,'$name');
      h(t,'$parent');
      h(t,'$module');
      h(t,'$initSpec');
    }
    initfn.call(t);
    if (!t.$new){
      t.$new = function(){ return Object.create(t); };
    }
    t.$clone = function(r){ return t.$new().$assign(r); };
    h(t,'$new');
    h(t,'$clone');
    h(t,'$eq');
    h(t,'$assign');
    return t;
  },

  is: function(instance,type){
    return type.isPrototypeOf(instance) || (instance===type);
  },

  isExt: function(instance,type,mode){
    // mode===1 means instance must be a Pascal class instance
    // mode===2 means instance must be a Pascal class
    // Notes:
    // isPrototypeOf and instanceof return false on equal
    // isPrototypeOf does not work for Date.isPrototypeOf(new Date())
    //   so if isPrototypeOf is false test with instanceof
    // instanceof needs a function on right side
    if (instance == null) return false; // Note: ==null checks for undefined too
    if ((typeof(type) !== 'object') && (typeof(type) !== 'function')) return false;
    if (instance === type){
      if (mode===1) return false;
      if (mode===2) return rtl.isPasClass(instance);
      return true;
    }
    if (type.isPrototypeOf && type.isPrototypeOf(instance)){
      if (mode===1) return rtl.isPasClassInstance(instance);
      if (mode===2) return rtl.isPasClass(instance);
      return true;
    }
    if ((typeof type == 'function') && (instance instanceof type)) return true;
    return false;
  },

  Exception: null,
  EInvalidCast: null,
  EAbstractError: null,
  ERangeError: null,
  EIntOverflow: null,
  EPropWriteOnly: null,

  raiseE: function(typename){
    var t = rtl[typename];
    if (t==null){
      var mod = pas.SysUtils;
      if (!mod) mod = pas.sysutils;
      if (mod){
        t = mod[typename];
        if (!t) t = mod[typename.toLowerCase()];
        if (!t) t = mod['Exception'];
        if (!t) t = mod['exception'];
      }
    }
    if (t){
      if (t.Create){
        throw t.$create("Create");
      } else if (t.create){
        throw t.$create("create");
      }
    }
    if (typename === "EInvalidCast") throw "invalid type cast";
    if (typename === "EAbstractError") throw "Abstract method called";
    if (typename === "ERangeError") throw "range error";
    throw typename;
  },

  as: function(instance,type){
    if((instance === null) || rtl.is(instance,type)) return instance;
    rtl.raiseE("EInvalidCast");
  },

  asExt: function(instance,type,mode){
    if((instance === null) || rtl.isExt(instance,type,mode)) return instance;
    rtl.raiseE("EInvalidCast");
  },

  createInterface: function(module, name, guid, fnnames, ancestor, initfn){
    //console.log('createInterface name="'+name+'" guid="'+guid+'" names='+fnnames);
    var i = ancestor?Object.create(ancestor):{};
    module[name] = i;
    i.$module = module;
    i.$name = name;
    i.$fullname = module.$name+'.'+name;
    i.$guid = guid;
    i.$guidr = null;
    i.$names = fnnames?fnnames:[];
    if (rtl.isFunction(initfn)){
      // rtti
      if (rtl.debug_rtti) rtl.debug('createInterface '+i.$fullname);
      var t = i.$module.$rtti.$Interface(name,{ "interface": i, module: module });
      i.$rtti = t;
      if (ancestor) t.ancestor = ancestor.$rtti;
      if (!t.ancestor) t.ancestor = null;
      initfn.call(i);
    }
    return i;
  },

  strToGUIDR: function(s,g){
    var p = 0;
    function n(l){
      var h = s.substr(p,l);
      p+=l;
      return parseInt(h,16);
    }
    p+=1; // skip {
    g.D1 = n(8);
    p+=1; // skip -
    g.D2 = n(4);
    p+=1; // skip -
    g.D3 = n(4);
    p+=1; // skip -
    if (!g.D4) g.D4=[];
    g.D4[0] = n(2);
    g.D4[1] = n(2);
    p+=1; // skip -
    for(var i=2; i<8; i++) g.D4[i] = n(2);
    return g;
  },

  guidrToStr: function(g){
    if (g.$intf) return g.$intf.$guid;
    var h = rtl.hexStr;
    var s='{'+h(g.D1,8)+'-'+h(g.D2,4)+'-'+h(g.D3,4)+'-'+h(g.D4[0],2)+h(g.D4[1],2)+'-';
    for (var i=2; i<8; i++) s+=h(g.D4[i],2);
    s+='}';
    return s;
  },

  createTGUID: function(guid){
    var TGuid = (pas.System)?pas.System.TGuid:pas.system.tguid;
    var g = rtl.strToGUIDR(guid,TGuid.$new());
    return g;
  },

  getIntfGUIDR: function(intfTypeOrVar){
    if (!intfTypeOrVar) return null;
    if (!intfTypeOrVar.$guidr){
      var g = rtl.createTGUID(intfTypeOrVar.$guid);
      if (!intfTypeOrVar.hasOwnProperty('$guid')) intfTypeOrVar = Object.getPrototypeOf(intfTypeOrVar);
      g.$intf = intfTypeOrVar;
      intfTypeOrVar.$guidr = g;
    }
    return intfTypeOrVar.$guidr;
  },

  addIntf: function (aclass, intf, map){
    function jmp(fn){
      if (typeof(fn)==="function"){
        return function(){ return fn.apply(this.$o,arguments); };
      } else {
        return function(){ rtl.raiseE('EAbstractError'); };
      }
    }
    if(!map) map = {};
    var t = intf;
    var item = Object.create(t);
    if (!aclass.hasOwnProperty('$intfmaps')) aclass.$intfmaps = {};
    aclass.$intfmaps[intf.$guid] = item;
    do{
      var names = t.$names;
      if (!names) break;
      for (var i=0; i<names.length; i++){
        var intfname = names[i];
        var fnname = map[intfname];
        if (!fnname) fnname = intfname;
        //console.log('addIntf: intftype='+t.$name+' index='+i+' intfname="'+intfname+'" fnname="'+fnname+'" old='+typeof(item[intfname]));
        item[intfname] = jmp(aclass[fnname]);
      }
      t = Object.getPrototypeOf(t);
    }while(t!=null);
  },

  getIntfG: function (obj, guid, query){
    if (!obj) return null;
    //console.log('getIntfG: obj='+obj.$classname+' guid='+guid+' query='+query);
    // search
    var maps = obj.$intfmaps;
    if (!maps) return null;
    var item = maps[guid];
    if (!item) return null;
    // check delegation
    //console.log('getIntfG: obj='+obj.$classname+' guid='+guid+' query='+query+' item='+typeof(item));
    if (typeof item === 'function') return item.call(obj); // delegate. Note: COM contains _AddRef
    // check cache
    var intf = null;
    if (obj.$interfaces){
      intf = obj.$interfaces[guid];
      //console.log('getIntfG: obj='+obj.$classname+' guid='+guid+' cache='+typeof(intf));
    }
    if (!intf){ // intf can be undefined!
      intf = Object.create(item);
      intf.$o = obj;
      if (!obj.$interfaces) obj.$interfaces = {};
      obj.$interfaces[guid] = intf;
    }
    if (typeof(query)==='object'){
      // called by queryIntfT
      var o = null;
      if (intf.QueryInterface(rtl.getIntfGUIDR(query),
          {get:function(){ return o; }, set:function(v){ o=v; }}) === 0){
        return o;
      } else {
        return null;
      }
    } else if(query===2){
      // called by TObject.GetInterfaceByStr
      if (intf.$kind === 'com') intf._AddRef();
    }
    return intf;
  },

  getIntfT: function(obj,intftype){
    return rtl.getIntfG(obj,intftype.$guid);
  },

  queryIntfT: function(obj,intftype){
    return rtl.getIntfG(obj,intftype.$guid,intftype);
  },

  queryIntfIsT: function(obj,intftype){
    var i = rtl.getIntfG(obj,intftype.$guid);
    if (!i) return false;
    if (i.$kind === 'com') i._Release();
    return true;
  },

  asIntfT: function (obj,intftype){
    var i = rtl.getIntfG(obj,intftype.$guid);
    if (i!==null) return i;
    rtl.raiseEInvalidCast();
  },

  intfIsIntfT: function(intf,intftype){
    return (intf!==null) && rtl.queryIntfIsT(intf.$o,intftype);
  },

  intfAsIntfT: function (intf,intftype){
    if (!intf) return null;
    var i = rtl.getIntfG(intf.$o,intftype.$guid);
    if (i) return i;
    rtl.raiseEInvalidCast();
  },

  intfIsClass: function(intf,classtype){
    return (intf!=null) && (rtl.is(intf.$o,classtype));
  },

  intfAsClass: function(intf,classtype){
    if (intf==null) return null;
    return rtl.as(intf.$o,classtype);
  },

  intfToClass: function(intf,classtype){
    if ((intf!==null) && rtl.is(intf.$o,classtype)) return intf.$o;
    return null;
  },

  // interface reference counting
  intfRefs: { // base object for temporary interface variables
    ref: function(id,intf){
      // called for temporary interface references needing delayed release
      var old = this[id];
      //console.log('rtl.intfRefs.ref: id='+id+' old="'+(old?old.$name:'null')+'" intf="'+(intf?intf.$name:'null')+' $o='+(intf?intf.$o:'null'));
      if (old){
        // called again, e.g. in a loop
        delete this[id];
        old._Release(); // may fail
      }
      if(intf) {
        this[id]=intf;
      }
      return intf;
    },
    free: function(){
      //console.log('rtl.intfRefs.free...');
      for (var id in this){
        if (this.hasOwnProperty(id)){
          var intf = this[id];
          if (intf){
            //console.log('rtl.intfRefs.free: id='+id+' '+intf.$name+' $o='+intf.$o.$classname);
            intf._Release();
          }
        }
      }
    }
  },

  createIntfRefs: function(){
    //console.log('rtl.createIntfRefs');
    return Object.create(rtl.intfRefs);
  },

  setIntfP: function(path,name,value,skipAddRef){
    var old = path[name];
    //console.log('rtl.setIntfP path='+path+' name='+name+' old="'+(old?old.$name:'null')+'" value="'+(value?value.$name:'null')+'"');
    if (old === value) return;
    if (old !== null){
      path[name]=null;
      old._Release();
    }
    if (value !== null){
      if (!skipAddRef) value._AddRef();
      path[name]=value;
    }
  },

  setIntfL: function(old,value,skipAddRef){
    //console.log('rtl.setIntfL old="'+(old?old.$name:'null')+'" value="'+(value?value.$name:'null')+'"');
    if (old !== value){
      if (value!==null){
        if (!skipAddRef) value._AddRef();
      }
      if (old!==null){
        old._Release();  // Release after AddRef, to avoid double Release if Release creates an exception
      }
    } else if (skipAddRef){
      if (old!==null){
        old._Release();  // value has an AddRef
      }
    }
    return value;
  },

  _AddRef: function(intf){
    //if (intf) console.log('rtl._AddRef intf="'+(intf?intf.$name:'null')+'"');
    if (intf) intf._AddRef();
    return intf;
  },

  _Release: function(intf){
    //if (intf) console.log('rtl._Release intf="'+(intf?intf.$name:'null')+'"');
    if (intf) intf._Release();
    return intf;
  },

  trunc: function(a){
    return a<0 ? Math.ceil(a) : Math.floor(a);
  },

  checkMethodCall: function(obj,type){
    if (rtl.isObject(obj) && rtl.is(obj,type)) return;
    rtl.raiseE("EInvalidCast");
  },

  oc: function(i){
    // overflow check integer
    if ((Math.floor(i)===i) && (i>=-0x1fffffffffffff) && (i<=0x1fffffffffffff)) return i;
    rtl.raiseE('EIntOverflow');
  },

  rc: function(i,minval,maxval){
    // range check integer
    if ((Math.floor(i)===i) && (i>=minval) && (i<=maxval)) return i;
    rtl.raiseE('ERangeError');
  },

  rcc: function(c,minval,maxval){
    // range check char
    if ((typeof(c)==='string') && (c.length===1)){
      var i = c.charCodeAt(0);
      if ((i>=minval) && (i<=maxval)) return c;
    }
    rtl.raiseE('ERangeError');
  },

  rcSetCharAt: function(s,index,c){
    // range check setCharAt
    if ((typeof(s)!=='string') || (index<0) || (index>=s.length)) rtl.raiseE('ERangeError');
    return rtl.setCharAt(s,index,c);
  },

  rcCharAt: function(s,index){
    // range check charAt
    if ((typeof(s)!=='string') || (index<0) || (index>=s.length)) rtl.raiseE('ERangeError');
    return s.charAt(index);
  },

  rcArrR: function(arr,index){
    // range check read array
    if (Array.isArray(arr) && (typeof(index)==='number') && (index>=0) && (index<arr.length)){
      if (arguments.length>2){
        // arr,index1,index2,...
        arr=arr[index];
        for (var i=2; i<arguments.length; i++) arr=rtl.rcArrR(arr,arguments[i]);
        return arr;
      }
      return arr[index];
    }
    rtl.raiseE('ERangeError');
  },

  rcArrW: function(arr,index,value){
    // range check write array
    // arr,index1,index2,...,value
    for (var i=3; i<arguments.length; i++){
      arr=rtl.rcArrR(arr,index);
      index=arguments[i-1];
      value=arguments[i];
    }
    if (Array.isArray(arr) && (typeof(index)==='number') && (index>=0) && (index<arr.length)){
      return arr[index]=value;
    }
    rtl.raiseE('ERangeError');
  },

  length: function(arr){
    return (arr == null) ? 0 : arr.length;
  },

  arrayRef: function(a){
    if (a!=null) rtl.hideProp(a,'$pas2jsrefcnt',1);
    return a;
  },

  arraySetLength: function(arr,defaultvalue,newlength){
    var stack = [];
    var s = 9999;
    for (var i=2; i<arguments.length; i++){
      var j = arguments[i];
      if (j==='s'){ s = i-2; }
      else {
        stack.push({ dim:j+0, a:null, i:0, src:null });
      }
    }
    var dimmax = stack.length-1;
    var depth = 0;
    var lastlen = 0;
    var item = null;
    var a = null;
    var src = arr;
    var srclen = 0, oldlen = 0;
    do{
      if (depth>0){
        item=stack[depth-1];
        src = (item.src && item.src.length>item.i)?item.src[item.i]:null;
      }
      if (!src){
        a = [];
        srclen = 0;
        oldlen = 0;
      } else if (src.$pas2jsrefcnt>0 || depth>=s){
        a = [];
        srclen = src.length;
        oldlen = srclen;
      } else {
        a = src;
        srclen = 0;
        oldlen = a.length;
      }
      lastlen = stack[depth].dim;
      a.length = lastlen;
      if (depth>0){
        item.a[item.i]=a;
        item.i++;
        if ((lastlen===0) && (item.i<item.a.length)) continue;
      }
      if (lastlen>0){
        if (depth<dimmax){
          item = stack[depth];
          item.a = a;
          item.i = 0;
          item.src = src;
          depth++;
          continue;
        } else {
          if (srclen>lastlen) srclen=lastlen;
          if (rtl.isArray(defaultvalue)){
            // array of dyn array
            for (var i=0; i<srclen; i++) a[i]=src[i];
            for (var i=oldlen; i<lastlen; i++) a[i]=[];
          } else if (rtl.isObject(defaultvalue)) {
            if (rtl.isTRecord(defaultvalue)){
              // array of record
              for (var i=0; i<srclen; i++) a[i]=defaultvalue.$clone(src[i]);
              for (var i=oldlen; i<lastlen; i++) a[i]=defaultvalue.$new();
            } else {
              // array of set
              for (var i=0; i<srclen; i++) a[i]=rtl.refSet(src[i]);
              for (var i=oldlen; i<lastlen; i++) a[i]={};
            }
          } else {
            for (var i=0; i<srclen; i++) a[i]=src[i];
            for (var i=oldlen; i<lastlen; i++) a[i]=defaultvalue;
          }
        }
      }
      // backtrack
      while ((depth>0) && (stack[depth-1].i>=stack[depth-1].dim)){
        depth--;
      };
      if (depth===0){
        if (dimmax===0) return a;
        return stack[0].a;
      }
    }while (true);
  },

  arrayEq: function(a,b){
    if (a===null) return b===null;
    if (b===null) return false;
    if (a.length!==b.length) return false;
    for (var i=0; i<a.length; i++) if (a[i]!==b[i]) return false;
    return true;
  },

  arrayClone: function(type,src,srcpos,endpos,dst,dstpos){
    // type: 0 for references, "refset" for calling refSet(), a function for new type()
    // src must not be null
    // This function does not range check.
    if(type === 'refSet') {
      for (; srcpos<endpos; srcpos++) dst[dstpos++] = rtl.refSet(src[srcpos]); // ref set
    } else if (rtl.isTRecord(type)){
      for (; srcpos<endpos; srcpos++) dst[dstpos++] = type.$clone(src[srcpos]); // clone record
    }  else {
      for (; srcpos<endpos; srcpos++) dst[dstpos++] = src[srcpos]; // reference
    };
  },

  arrayConcat: function(type){
    // type: see rtl.arrayClone
    var a = [];
    var l = 0;
    for (var i=1; i<arguments.length; i++){
      var src = arguments[i];
      if (src !== null) l+=src.length;
    };
    a.length = l;
    l=0;
    for (var i=1; i<arguments.length; i++){
      var src = arguments[i];
      if (src === null) continue;
      rtl.arrayClone(type,src,0,src.length,a,l);
      l+=src.length;
    };
    return a;
  },

  arrayConcatN: function(){
    var a = null;
    for (var i=0; i<arguments.length; i++){
      var src = arguments[i];
      if (src === null) continue;
      if (a===null){
        a=rtl.arrayRef(src); // Note: concat(a) does not clone
      } else {
        a=a.concat(src);
      }
    };
    return a;
  },

  arrayCopy: function(type, srcarray, index, count){
    // type: see rtl.arrayClone
    // if count is missing, use srcarray.length
    if (srcarray === null) return [];
    if (index < 0) index = 0;
    if (count === undefined) count=srcarray.length;
    var end = index+count;
    if (end>srcarray.length) end = srcarray.length;
    if (index>=end) return [];
    if (type===0){
      return srcarray.slice(index,end);
    } else {
      var a = [];
      a.length = end-index;
      rtl.arrayClone(type,srcarray,index,end,a,0);
      return a;
    }
  },

  arrayInsert: function(item, arr, index){
    if (arr){
      arr.splice(index,0,item);
      return arr;
    } else {
      return [item];
    }
  },

  setCharAt: function(s,index,c){
    return s.substr(0,index)+c+s.substr(index+1);
  },

  getResStr: function(mod,name){
    var rs = mod.$resourcestrings[name];
    return rs.current?rs.current:rs.org;
  },

  createSet: function(){
    var s = {};
    for (var i=0; i<arguments.length; i++){
      if (arguments[i]!=null){
        s[arguments[i]]=true;
      } else {
        var first=arguments[i+=1];
        var last=arguments[i+=1];
        for(var j=first; j<=last; j++) s[j]=true;
      }
    }
    return s;
  },

  cloneSet: function(s){
    var r = {};
    for (var key in s) r[key]=true;
    return r;
  },

  refSet: function(s){
    rtl.hideProp(s,'$shared',true);
    return s;
  },

  includeSet: function(s,enumvalue){
    if (s.$shared) s = rtl.cloneSet(s);
    s[enumvalue] = true;
    return s;
  },

  excludeSet: function(s,enumvalue){
    if (s.$shared) s = rtl.cloneSet(s);
    delete s[enumvalue];
    return s;
  },

  diffSet: function(s,t){
    var r = {};
    for (var key in s) if (!t[key]) r[key]=true;
    return r;
  },

  unionSet: function(s,t){
    var r = {};
    for (var key in s) r[key]=true;
    for (var key in t) r[key]=true;
    return r;
  },

  intersectSet: function(s,t){
    var r = {};
    for (var key in s) if (t[key]) r[key]=true;
    return r;
  },

  symDiffSet: function(s,t){
    var r = {};
    for (var key in s) if (!t[key]) r[key]=true;
    for (var key in t) if (!s[key]) r[key]=true;
    return r;
  },

  eqSet: function(s,t){
    for (var key in s) if (!t[key]) return false;
    for (var key in t) if (!s[key]) return false;
    return true;
  },

  neSet: function(s,t){
    return !rtl.eqSet(s,t);
  },

  leSet: function(s,t){
    for (var key in s) if (!t[key]) return false;
    return true;
  },

  geSet: function(s,t){
    for (var key in t) if (!s[key]) return false;
    return true;
  },

  strSetLength: function(s,newlen){
    var oldlen = s.length;
    if (oldlen > newlen){
      return s.substring(0,newlen);
    } else if (s.repeat){
      // Note: repeat needs ECMAScript6!
      return s+' '.repeat(newlen-oldlen);
    } else {
       while (oldlen<newlen){
         s+=' ';
         oldlen++;
       };
       return s;
    }
  },

  spaceLeft: function(s,width){
    var l=s.length;
    if (l>=width) return s;
    if (s.repeat){
      // Note: repeat needs ECMAScript6!
      return ' '.repeat(width-l) + s;
    } else {
      while (l<width){
        s=' '+s;
        l++;
      };
      return s;
    };
  },

  floatToStr: function(d,w,p){
    // input 1-3 arguments: double, width, precision
    if (arguments.length>2){
      return rtl.spaceLeft(d.toFixed(p),w);
    } else {
	  // exponent width
	  var pad = "";
	  var ad = Math.abs(d);
	  if (ad<1.0e+10) {
		pad='00';
	  } else if (ad<1.0e+100) {
		pad='0';
      }  	
	  if (arguments.length<2) {
	    w=9;		
      } else if (w<9) {
		w=9;
      }		  
      var p = w-8;
      var s=(d>0 ? " " : "" ) + d.toExponential(p);
      s=s.replace(/e(.)/,'E$1'+pad);
      return rtl.spaceLeft(s,w);
    }
  },

  valEnum: function(s, enumType, setCodeFn){
    s = s.toLowerCase();
    for (var key in enumType){
      if((typeof(key)==='string') && (key.toLowerCase()===s)){
        setCodeFn(0);
        return enumType[key];
      }
    }
    setCodeFn(1);
    return 0;
  },

  lw: function(l){
    // fix longword bitwise operation
    return l<0?l+0x100000000:l;
  },

  and: function(a,b){
    var hi = 0x80000000;
    var low = 0x7fffffff;
    var h = (a / hi) & (b / hi);
    var l = (a & low) & (b & low);
    return h*hi + l;
  },

  or: function(a,b){
    var hi = 0x80000000;
    var low = 0x7fffffff;
    var h = (a / hi) | (b / hi);
    var l = (a & low) | (b & low);
    return h*hi + l;
  },

  xor: function(a,b){
    var hi = 0x80000000;
    var low = 0x7fffffff;
    var h = (a / hi) ^ (b / hi);
    var l = (a & low) ^ (b & low);
    return h*hi + l;
  },

  shr: function(a,b){
    if (a<0) a += rtl.hiInt;
    if (a<0x80000000) return a >> b;
    if (b<=0) return a;
    if (b>54) return 0;
    return Math.floor(a / Math.pow(2,b));
  },

  shl: function(a,b){
    if (a<0) a += rtl.hiInt;
    if (b<=0) return a;
    if (b>54) return 0;
    var r = a * Math.pow(2,b);
    if (r <= rtl.hiInt) return r;
    return r % rtl.hiInt;
  },

  initRTTI: function(){
    if (rtl.debug_rtti) rtl.debug('initRTTI');

    // base types
    rtl.tTypeInfo = { name: "tTypeInfo" };
    function newBaseTI(name,kind,ancestor){
      if (!ancestor) ancestor = rtl.tTypeInfo;
      if (rtl.debug_rtti) rtl.debug('initRTTI.newBaseTI "'+name+'" '+kind+' ("'+ancestor.name+'")');
      var t = Object.create(ancestor);
      t.name = name;
      t.kind = kind;
      rtl[name] = t;
      return t;
    };
    function newBaseInt(name,minvalue,maxvalue,ordtype){
      var t = newBaseTI(name,1 /* tkInteger */,rtl.tTypeInfoInteger);
      t.minvalue = minvalue;
      t.maxvalue = maxvalue;
      t.ordtype = ordtype;
      return t;
    };
    newBaseTI("tTypeInfoInteger",1 /* tkInteger */);
    newBaseInt("shortint",-0x80,0x7f,0);
    newBaseInt("byte",0,0xff,1);
    newBaseInt("smallint",-0x8000,0x7fff,2);
    newBaseInt("word",0,0xffff,3);
    newBaseInt("longint",-0x80000000,0x7fffffff,4);
    newBaseInt("longword",0,0xffffffff,5);
    newBaseInt("nativeint",-0x10000000000000,0xfffffffffffff,6);
    newBaseInt("nativeuint",0,0xfffffffffffff,7);
    newBaseTI("char",2 /* tkChar */);
    newBaseTI("string",3 /* tkString */);
    newBaseTI("tTypeInfoEnum",4 /* tkEnumeration */,rtl.tTypeInfoInteger);
    newBaseTI("tTypeInfoSet",5 /* tkSet */);
    newBaseTI("double",6 /* tkDouble */);
    newBaseTI("boolean",7 /* tkBool */);
    newBaseTI("tTypeInfoProcVar",8 /* tkProcVar */);
    newBaseTI("tTypeInfoMethodVar",9 /* tkMethod */,rtl.tTypeInfoProcVar);
    newBaseTI("tTypeInfoArray",10 /* tkArray */);
    newBaseTI("tTypeInfoDynArray",11 /* tkDynArray */);
    newBaseTI("tTypeInfoPointer",15 /* tkPointer */);
    var t = newBaseTI("pointer",15 /* tkPointer */,rtl.tTypeInfoPointer);
    t.reftype = null;
    newBaseTI("jsvalue",16 /* tkJSValue */);
    newBaseTI("tTypeInfoRefToProcVar",17 /* tkRefToProcVar */,rtl.tTypeInfoProcVar);

    // member kinds
    rtl.tTypeMember = {};
    function newMember(name,kind){
      var m = Object.create(rtl.tTypeMember);
      m.name = name;
      m.kind = kind;
      rtl[name] = m;
    };
    newMember("tTypeMemberField",1); // tmkField
    newMember("tTypeMemberMethod",2); // tmkMethod
    newMember("tTypeMemberProperty",3); // tmkProperty

    // base object for storing members: a simple object
    rtl.tTypeMembers = {};

    // tTypeInfoStruct - base object for tTypeInfoClass, tTypeInfoRecord, tTypeInfoInterface
    var tis = newBaseTI("tTypeInfoStruct",0);
    tis.$addMember = function(name,ancestor,options){
      if (rtl.debug_rtti){
        if (!rtl.hasString(name) || (name.charAt()==='$')) throw 'invalid member "'+name+'", this="'+this.name+'"';
        if (!rtl.is(ancestor,rtl.tTypeMember)) throw 'invalid ancestor "'+ancestor+':'+ancestor.name+'", "'+this.name+'.'+name+'"';
        if ((options!=undefined) && (typeof(options)!='object')) throw 'invalid options "'+options+'", "'+this.name+'.'+name+'"';
      };
      var t = Object.create(ancestor);
      t.name = name;
      this.members[name] = t;
      this.names.push(name);
      if (rtl.isObject(options)){
        for (var key in options) if (options.hasOwnProperty(key)) t[key] = options[key];
      };
      return t;
    };
    tis.addField = function(name,type,options){
      var t = this.$addMember(name,rtl.tTypeMemberField,options);
      if (rtl.debug_rtti){
        if (!rtl.is(type,rtl.tTypeInfo)) throw 'invalid type "'+type+'", "'+this.name+'.'+name+'"';
      };
      t.typeinfo = type;
      this.fields.push(name);
      return t;
    };
    tis.addFields = function(){
      var i=0;
      while(i<arguments.length){
        var name = arguments[i++];
        var type = arguments[i++];
        if ((i<arguments.length) && (typeof(arguments[i])==='object')){
          this.addField(name,type,arguments[i++]);
        } else {
          this.addField(name,type);
        };
      };
    };
    tis.addMethod = function(name,methodkind,params,result,options){
      var t = this.$addMember(name,rtl.tTypeMemberMethod,options);
      t.methodkind = methodkind;
      t.procsig = rtl.newTIProcSig(params);
      t.procsig.resulttype = result?result:null;
      this.methods.push(name);
      return t;
    };
    tis.addProperty = function(name,flags,result,getter,setter,options){
      var t = this.$addMember(name,rtl.tTypeMemberProperty,options);
      t.flags = flags;
      t.typeinfo = result;
      t.getter = getter;
      t.setter = setter;
      // Note: in options: params, stored, defaultvalue
      if (rtl.isArray(t.params)) t.params = rtl.newTIParams(t.params);
      this.properties.push(name);
      if (!rtl.isString(t.stored)) t.stored = "";
      return t;
    };
    tis.getField = function(index){
      return this.members[this.fields[index]];
    };
    tis.getMethod = function(index){
      return this.members[this.methods[index]];
    };
    tis.getProperty = function(index){
      return this.members[this.properties[index]];
    };

    newBaseTI("tTypeInfoRecord",12 /* tkRecord */,rtl.tTypeInfoStruct);
    newBaseTI("tTypeInfoClass",13 /* tkClass */,rtl.tTypeInfoStruct);
    newBaseTI("tTypeInfoClassRef",14 /* tkClassRef */);
    newBaseTI("tTypeInfoInterface",18 /* tkInterface */,rtl.tTypeInfoStruct);
    newBaseTI("tTypeInfoHelper",19 /* tkHelper */,rtl.tTypeInfoStruct);
    newBaseTI("tTypeInfoExtClass",20 /* tkExtClass */,rtl.tTypeInfoClass);
  },

  tSectionRTTI: {
    $module: null,
    $inherited: function(name,ancestor,o){
      if (rtl.debug_rtti){
        rtl.debug('tSectionRTTI.newTI "'+(this.$module?this.$module.$name:"(no module)")
          +'"."'+name+'" ('+ancestor.name+') '+(o?'init':'forward'));
      };
      var t = this[name];
      if (t){
        if (!t.$forward) throw 'duplicate type "'+name+'"';
        if (!ancestor.isPrototypeOf(t)) throw 'typeinfo ancestor mismatch "'+name+'" ancestor="'+ancestor.name+'" t.name="'+t.name+'"';
      } else {
        t = Object.create(ancestor);
        t.name = name;
        t.$module = this.$module;
        this[name] = t;
      }
      if (o){
        delete t.$forward;
        for (var key in o) if (o.hasOwnProperty(key)) t[key]=o[key];
      } else {
        t.$forward = true;
      }
      return t;
    },
    $Scope: function(name,ancestor,o){
      var t=this.$inherited(name,ancestor,o);
      t.members = {};
      t.names = [];
      t.fields = [];
      t.methods = [];
      t.properties = [];
      return t;
    },
    $TI: function(name,kind,o){ var t=this.$inherited(name,rtl.tTypeInfo,o); t.kind = kind; return t; },
    $Int: function(name,o){ return this.$inherited(name,rtl.tTypeInfoInteger,o); },
    $Enum: function(name,o){ return this.$inherited(name,rtl.tTypeInfoEnum,o); },
    $Set: function(name,o){ return this.$inherited(name,rtl.tTypeInfoSet,o); },
    $StaticArray: function(name,o){ return this.$inherited(name,rtl.tTypeInfoArray,o); },
    $DynArray: function(name,o){ return this.$inherited(name,rtl.tTypeInfoDynArray,o); },
    $ProcVar: function(name,o){ return this.$inherited(name,rtl.tTypeInfoProcVar,o); },
    $RefToProcVar: function(name,o){ return this.$inherited(name,rtl.tTypeInfoRefToProcVar,o); },
    $MethodVar: function(name,o){ return this.$inherited(name,rtl.tTypeInfoMethodVar,o); },
    $Record: function(name,o){ return this.$Scope(name,rtl.tTypeInfoRecord,o); },
    $Class: function(name,o){ return this.$Scope(name,rtl.tTypeInfoClass,o); },
    $ClassRef: function(name,o){ return this.$inherited(name,rtl.tTypeInfoClassRef,o); },
    $Pointer: function(name,o){ return this.$inherited(name,rtl.tTypeInfoPointer,o); },
    $Interface: function(name,o){ return this.$Scope(name,rtl.tTypeInfoInterface,o); },
    $Helper: function(name,o){ return this.$Scope(name,rtl.tTypeInfoHelper,o); },
    $ExtClass: function(name,o){ return this.$Scope(name,rtl.tTypeInfoExtClass,o); }
  },

  newTIParam: function(param){
    // param is an array, 0=name, 1=type, 2=optional flags
    var t = {
      name: param[0],
      typeinfo: param[1],
      flags: (rtl.isNumber(param[2]) ? param[2] : 0)
    };
    return t;
  },

  newTIParams: function(list){
    // list: optional array of [paramname,typeinfo,optional flags]
    var params = [];
    if (rtl.isArray(list)){
      for (var i=0; i<list.length; i++) params.push(rtl.newTIParam(list[i]));
    };
    return params;
  },

  newTIProcSig: function(params,result,flags){
    var s = {
      params: rtl.newTIParams(params),
      resulttype: result,
      flags: flags
    };
    return s;
  },

  addResource: function(aRes){
    rtl.$res[aRes.name]=aRes;
  },

  getResource: function(aName){
    var res = rtl.$res[aName];
    if (res !== undefined) {
      return res;
    } else {
      return null;
    }
  },

  getResourceList: function(){
    return Object.keys(rtl.$res);
  }
}

rtl.module("System",[],function () {
  "use strict";
  var $mod = this;
  var $impl = $mod.$impl;
  rtl.recNewT(this,"TGuid",function () {
    this.D1 = 0;
    this.D2 = 0;
    this.D3 = 0;
    this.$new = function () {
      var r = Object.create(this);
      r.D4 = rtl.arraySetLength(null,0,8);
      return r;
    };
    this.$eq = function (b) {
      return (this.D1 === b.D1) && (this.D2 === b.D2) && (this.D3 === b.D3) && rtl.arrayEq(this.D4,b.D4);
    };
    this.$assign = function (s) {
      this.D1 = s.D1;
      this.D2 = s.D2;
      this.D3 = s.D3;
      this.D4 = s.D4.slice(0);
      return this;
    };
  });
  rtl.createClass(this,"TObject",null,function () {
    this.$init = function () {
    };
    this.$final = function () {
    };
    this.Create = function () {
      return this;
    };
    this.AfterConstruction = function () {
    };
    this.BeforeDestruction = function () {
    };
  });
  this.Random = function (Range) {
    return Math.floor(Math.random()*Range);
  };
  this.Copy = function (S, Index, Size) {
    if (Index<1) Index = 1;
    return (Size>0) ? S.substring(Index-1,Index+Size-1) : "";
  };
  this.Writeln = function () {
    var i = 0;
    var l = 0;
    var s = "";
    l = arguments.length - 1;
    if ($impl.WriteCallBack != null) {
      for (var $l = 0, $end = l; $l <= $end; $l++) {
        i = $l;
        $impl.WriteCallBack(arguments[i],i === l);
      };
    } else {
      s = $impl.WriteBuf;
      for (var $l1 = 0, $end1 = l; $l1 <= $end1; $l1++) {
        i = $l1;
        s = s + ("" + arguments[i]);
      };
      console.log(s);
      $impl.WriteBuf = "";
    };
  };
  $mod.$implcode = function () {
    $impl.WriteBuf = "";
    $impl.WriteCallBack = null;
  };
  $mod.$init = function () {
    rtl.exitcode = 0;
  };
},[]);
rtl.module("JS",["System"],function () {
  "use strict";
  var $mod = this;
  this.isDefined = function (v) {
    return !(v == undefined);
  };
});
rtl.module("SysUtils",["System","JS"],function () {
  "use strict";
  var $mod = this;
  var $impl = $mod.$impl;
  rtl.createClass(this,"Exception",pas.System.TObject,function () {
    this.LogMessageOnCreate = false;
    this.$init = function () {
      pas.System.TObject.$init.call(this);
      this.fMessage = "";
    };
    this.Create$1 = function (Msg) {
      this.fMessage = Msg;
      if (this.LogMessageOnCreate) pas.System.Writeln("Created exception ",this.$classname," with message: ",Msg);
      return this;
    };
  });
  this.TStringReplaceFlag = {"0": "rfReplaceAll", rfReplaceAll: 0, "1": "rfIgnoreCase", rfIgnoreCase: 1};
  this.StringReplace = function (aOriginal, aSearch, aReplace, Flags) {
    var Result = "";
    var REFlags = "";
    var REString = "";
    REFlags = "";
    if (0 in Flags) REFlags = "g";
    if (1 in Flags) REFlags = REFlags + "i";
    REString = aSearch.replace(new RegExp($impl.RESpecials,"g"),"\\$1");
    Result = aOriginal.replace(new RegExp(REString,REFlags),aReplace);
    return Result;
  };
  this.IntToStr = function (Value) {
    var Result = "";
    Result = "" + Value;
    return Result;
  };
  this.GUIDToString = function (guid) {
    var Result = "";
    Result = rtl.guidrToStr(guid);
    return Result;
  };
  this.CreateGUID = function (GUID) {
    var Result = 0;
    function R(B) {
      var Result = 0;
      var v = 0;
      v = pas.System.Random(256);
      while (B > 1) {
        v = (v * 256) + pas.System.Random(256);
        B -= 1;
      };
      Result = v;
      return Result;
    };
    var I = 0;
    Result = 0;
    GUID.D1 = R(4);
    GUID.D2 = R(2);
    GUID.D3 = R(2);
    for (I = 0; I <= 7; I++) GUID.D4[I] = R(1);
    return Result;
  };
  rtl.createHelper(this,"TStringHelper",null,function () {
    this.Join$1 = function (Separator, Values) {
      var Result = "";
      Result = Values.join(Separator);
      return Result;
    };
  });
  $mod.$implcode = function () {
    $impl.RESpecials = "([\\$\\+\\[\\]\\(\\)\\\\\\.\\*\\^\\?])";
  };
},[]);
rtl.module("Classes",["System","SysUtils","JS"],function () {
  "use strict";
  var $mod = this;
  var $impl = $mod.$impl;
  $mod.$implcode = function () {
    $impl.ClassList = null;
  };
  $mod.$init = function () {
    $impl.ClassList = new Object();
  };
},[]);
rtl.module("Web",["System","JS"],function () {
  "use strict";
  var $mod = this;
});
rtl.module("Dygraph",["System","JS","Web"],function () {
  "use strict";
  var $mod = this;
});
rtl.module("OTUtils",["System","JS","SysUtils"],function () {
  "use strict";
  var $mod = this;
  this.JSArrayOfStr2Pas = function (A) {
    var Result = [];
    var I = 0;
    Result = rtl.arraySetLength(Result,"",A.length);
    for (var $l = 0, $end = A.length - 1; $l <= $end; $l++) {
      I = $l;
      if (!rtl.isString(A[I])) throw pas.SysUtils.Exception.$create("Create$1",["Entry " + pas.SysUtils.IntToStr(I) + " in cssclasses is not a string"]);
      Result[I] = "" + A[I];
    };
    return Result;
  };
  this.GetGUIDStr = function () {
    var Result = "";
    var GUID = pas.System.TGuid.$new();
    pas.SysUtils.CreateGUID(GUID);
    Result = pas.SysUtils.GUIDToString(GUID);
    Result = pas.System.Copy(Result,2,36);
    return Result;
  };
});
rtl.module("Generics.Collections",["System","Classes","SysUtils","JS"],function () {
  "use strict";
  var $mod = this;
  var $impl = $mod.$impl;
  this.TCollectionNotification = {"0": "cnAdded", cnAdded: 0, "1": "cnRemoved", cnRemoved: 1, "2": "cnExtracted", cnExtracted: 2};
  rtl.createClass(this,"EDictionary",pas.SysUtils.Exception,function () {
  });
  rtl.recNewT(this,"TPair$G3",function () {
    this.$eq = function (b) {
      return true;
    };
    this.$assign = function (s) {
      return this;
    };
  });
  rtl.createClass(this,"TEnumerable$G17",pas.System.TObject,function () {
  },"TEnumerable<Generics.Collections.TPair<System.String,OTObjTerm.TOTObjectClass>>");
  rtl.createClass(this,"TDictionary$G2",this.TEnumerable$G17,function () {
    this.$init = function () {
      $mod.TEnumerable$G17.$init.call(this);
      this.FMap = null;
      this.FOnKeyNotify = null;
      this.FOnValueNotify = null;
    };
    this.$final = function () {
      this.FMap = undefined;
      this.FOnKeyNotify = undefined;
      this.FOnValueNotify = undefined;
      $mod.TEnumerable$G17.$final.call(this);
    };
    this.DoAdd = function (Key, Value) {
      this.FMap.set(Key,Value);
      this.KeyNotify(Key,0);
      this.ValueNotify(Value,0);
    };
    this.KeyNotify = function (Key, Action) {
      if (this.FOnKeyNotify != null) this.FOnKeyNotify(this,Key,Action);
    };
    this.ValueNotify = function (Value, Action) {
      if (this.FOnValueNotify != null) this.FOnValueNotify(this,Value,Action);
    };
    this.Create$1 = function (ACapacity) {
      this.FMap = new Map();
      return this;
    };
    this.Add = function (Key, Value) {
      if (this.FMap.has(Key)) throw $mod.EDictionary.$create("Create$1",[rtl.getResStr($mod,"SErrDictDuplicateKey")]);
      this.DoAdd(Key,Value);
    };
    this.TryGetValue = function (Key, Value) {
      var Result = false;
      Result = this.FMap.has(Key);
      if (Result) Value.set(rtl.getObject(this.FMap.get(Key)));
      return Result;
    };
  },"TDictionary<System.String,OTObjTerm.TOTObjectClass>");
  $mod.$implcode = function () {
    $mod.$resourcestrings = {SErrDictDuplicateKey: {org: "Duplicate key value"}};
  };
},[]);
rtl.module("OTObjTerm",["System","JS","Classes","SysUtils","Web","OTUtils","Generics.Collections"],function () {
  "use strict";
  var $mod = this;
  var $impl = $mod.$impl;
  rtl.createClass(this,"TOTObject",pas.System.TObject,function () {
    this.$init = function () {
      pas.System.TObject.$init.call(this);
      this.FObjTerm = null;
      this.FUniqueID = "";
      this.FCSSClasses = [];
      this.FCSSStyle = "";
      this.FElement = null;
    };
    this.$final = function () {
      this.FObjTerm = undefined;
      this.FCSSClasses = undefined;
      this.FElement = undefined;
      pas.System.TObject.$final.call(this);
    };
    this.Create$1 = function (AObjTerm, AUniqueID) {
      this.FObjTerm = AObjTerm;
      this.FUniqueID = AUniqueID;
      return this;
    };
    this.SetElementAndFill = function (AElement) {
      this.FElement = AElement;
    };
    this.ObjCmd = function (AMsg) {
      throw pas.SysUtils.Exception.$create("Create$1",[this.$classname + ".ObjCmd is not implemented"]);
    };
    this.CreateFromCmdAppend = function (AObjTerm, AUniqueID, AMsg) {
      var Result = null;
      throw pas.SysUtils.Exception.$create("Create$1",[this.$classname + ".CreateFromCmdAppend is not implemented"]);
      return Result;
    };
  });
  this.TMsgSeverity = {"0": "msNote", msNote: 0, "1": "msWarning", msWarning: 1, "2": "msError", msError: 2, "3": "msFatal", msFatal: 3};
  rtl.createClass(this,"TObjTerm",pas.System.TObject,function () {
    this.$init = function () {
      pas.System.TObject.$init.call(this);
      this.FHead = null;
      this.FTerm = null;
      this.FFoot = null;
      this.FWSURL = null;
      this.FWSConn = null;
      this.FShowBoxes = null;
      this.FInput = null;
      this.FObjects = null;
      this.FWS = null;
      this.FWSConnecting = false;
      this.FWSClosing = false;
    };
    this.$final = function () {
      this.FHead = undefined;
      this.FTerm = undefined;
      this.FFoot = undefined;
      this.FWSURL = undefined;
      this.FWSConn = undefined;
      this.FShowBoxes = undefined;
      this.FInput = undefined;
      this.FObjects = undefined;
      this.FWS = undefined;
      pas.System.TObject.$final.call(this);
    };
    this.Create$1 = function () {
      this.FHead = document.getElementById("othead");
      this.FTerm = document.getElementById("otterm");
      this.FFoot = document.getElementById("otfoot");
      this.FWSURL = document.getElementById("otwsurl");
      this.FWSConn = document.getElementById("otwsconn");
      this.FShowBoxes = document.getElementById("otwithboxes");
      this.FInput = document.getElementById("otinput");
      this.FObjects = new Array();
      this.FWSConn.onclick = rtl.createSafeCallback(this,"OnWSConnClick");
      this.FShowBoxes.onclick = rtl.createSafeCallback(this,"OnWithBoxesClick");
      this.FInput.onkeydown = rtl.createSafeCallback(this,"OnOTInputKeyDown");
      return this;
    };
    this.AppendObj = function (AOTObject) {
      var Result = null;
      var Obj = null;
      var Box = null;
      var Meta = null;
      var St = "";
      Obj = document.createElement("div");
      St = AOTObject.$class.GetOTType();
      if (rtl.length(AOTObject.FCSSClasses) > 0) St = St + " " + pas.SysUtils.TStringHelper.Join$1(" ",AOTObject.FCSSClasses);
      Obj.className = "otobject " + St;
      if (AOTObject.FCSSStyle > "") Obj.setAttribute("style",AOTObject.FCSSStyle);
      Obj.id = "o" + pas.SysUtils.IntToStr(this.FObjects.length);
      Box = document.createElement("div");
      Box.className = "otbox";
      Box.id = "b" + pas.SysUtils.IntToStr(this.FObjects.length);
      Meta = document.createElement("div");
      Meta.className = "otmeta";
      Meta.innerHTML = "Entry " + pas.SysUtils.IntToStr(this.FObjects.length) + " (" + St + ")";
      Box.appendChild(Meta);
      Box.appendChild(Obj);
      this.FTerm.appendChild(Box);
      this.FObjects.push(AOTObject);
      AOTObject.SetElementAndFill(Obj);
      this.FTerm.scrollTop = this.FTerm.scrollHeight - this.FTerm.clientHeight;
      Result = AOTObject;
      return Result;
    };
    this.AppendOTHTML = function (AHTML) {
      var Result = null;
      Result = this.AppendObj(pas.OTOBase.TOTOHTML.$create("Create$2",[this,pas.OTUtils.GetGUIDStr(),AHTML]));
      return Result;
    };
    this.AppendMsg = function (ASeverity, AMessage) {
      var Result = null;
      var OS = null;
      OS = pas.OTOBase.TOTOString.$create("Create$3",[this,pas.OTUtils.GetGUIDStr(),AMessage]);
      OS.FCSSClasses = [$impl.CMsgSeverityCSSClass[ASeverity]];
      Result = this.AppendObj(OS);
      return Result;
    };
    this.FindObj = function (AUniqueID) {
      var Result = null;
      var I = 0;
      for (var $l = 0, $end = this.FObjects.length - 1; $l <= $end; $l++) {
        I = $l;
        if (rtl.getObject(this.FObjects[I]).FUniqueID === AUniqueID) return rtl.getObject(this.FObjects[I]);
      };
      Result = null;
      return Result;
    };
    this.OnWSConnClick = function (Event) {
      var Result = false;
      if (Event.target.id !== "otwsconn") {
        this.AppendMsg(3,"ObjTerm: OnWSConnClick was called from invalid element id '" + Event.target.id + "'");
        return true;
      };
      if (this.FWSConn.checked) {
        this.Connect(this.FWSURL.value);
      } else {
        this.Disconnect();
      };
      Result = true;
      return Result;
    };
    this.OnWithBoxesClick = function (Event) {
      var Result = false;
      if (!this.FShowBoxes.checked) {
        this.BoxesHide()}
       else this.BoxesShow();
      Result = true;
      return Result;
    };
    this.OnOTInputKeyDown = function (Event) {
      var Result = false;
      var E = null;
      if (Event.key !== "Enter") return true;
      if (Event.target.id !== "otinput") {
        this.AppendMsg(3,"ObjTerm: OnOTInputKeyDown was called from invalid element id '" + Event.target.id + "'");
        return true;
      };
      E = Event.target;
      this.SendMessage(this.CreateMsgInput(E.value));
      E.value = "";
      Result = false;
      return Result;
    };
    this.InputEnable = function (APrompt, APromptStyle, AInputStyle) {
      var E = null;
      pas.System.Writeln("Enabling input with ",APrompt," ",APromptStyle," ",AInputStyle);
      E = document.getElementById("otprompt");
      E.innerHTML = APrompt;
      E.setAttribute("style",APromptStyle);
      E = document.getElementById("otinput");
      E.setAttribute("style",AInputStyle);
      E.textContent = "";
      E.focus();
      E = document.getElementById("otinputbar");
      E.style.setProperty("display","block");
    };
    this.InputDisable = function () {
      var E = null;
      E = document.getElementById("otinputbar");
      E.style.setProperty("display","none");
    };
    this.BoxesShow = function () {
      this.FTerm.classList.remove("othidebox");
    };
    this.BoxesHide = function () {
      this.FTerm.classList.add("othidebox");
    };
    this.HandleMessage = function (AMsg) {
      var MsgType = "";
      if (!pas.JS.isDefined(AMsg["msgtype"])) {
        this.AppendMsg(2,'ObjTerm: Received message without a message type ("' + JSON.stringify(AMsg) + '")');
        return;
      };
      MsgType = "" + AMsg["msgtype"];
      var $tmp = MsgType;
      if ($tmp === "helloreply") {
        this.HandleHelloReply(AMsg)}
       else if ($tmp === "cmd") {
        this.HandleCmd(AMsg)}
       else {
        this.AppendMsg(2,'ObjTerm: Received message with invalid message type "' + MsgType + '" ("' + JSON.stringify(AMsg) + '")');
      };
    };
    this.HandleHelloReply = function (AMsg) {
      var ClientVersion = "";
      var ClientIdent = "";
      var InputEnable = "";
      var InputPrompt = "";
      var PromptStyle = "";
      var InputStyle = "";
      ClientVersion = "" + AMsg["version"];
      ClientIdent = "" + AMsg["ident"];
      InputEnable = "" + AMsg["inputenable"];
      InputPrompt = "" + AMsg["inputprompt"];
      PromptStyle = "" + AMsg["promptstyle"];
      InputStyle = "" + AMsg["inputstyle"];
      if (InputEnable === "true") {
        this.InputEnable(InputPrompt,PromptStyle,InputStyle)}
       else this.InputDisable();
      this.AppendMsg(0,"ObjTerm: Client at " + this.FWS.url + " uses version " + ClientVersion + ", ident = " + ClientIdent);
    };
    this.HandleCmd = function (AMsg) {
      var Cmd = "";
      var CmdId = "";
      if (!pas.JS.isDefined(AMsg["cmdid"])) {
        this.AppendMsg(2,'ObjTerm: Received command message without command ID ("' + JSON.stringify(AMsg) + '")');
        return;
      };
      Cmd = "" + AMsg["cmd"];
      CmdId = "" + AMsg["cmdid"];
      var $tmp = Cmd;
      if ($tmp === "append") {
        this.HandleCmdAppend(AMsg)}
       else if ($tmp === "objcmd") {
        this.HandleCmdObjCmd(AMsg)}
       else {
        this.AppendMsg(2,'ObjTerm: Received invalid command "' + Cmd + '" ("' + JSON.stringify(AMsg) + '")');
        this.SendMessage(this.CreateMsgCmdStatus(CmdId,"error",'Invalid command "' + Cmd + '"'));
      };
    };
    this.HandleCmdAppend = function (AMsg) {
      var CmdId = "";
      var OTType = "";
      var UniqueID = "";
      var V = undefined;
      var A = null;
      var CSSClasses = [];
      var CSSStyle = "";
      var OTC = null;
      var OT = null;
      CmdId = "" + AMsg["cmdid"];
      OTType = "" + AMsg["type"];
      UniqueID = "" + AMsg["uniqueid"];
      V = AMsg["cssclasses"];
      if (pas.JS.isDefined(V)) {
        if (!rtl.isArray(V)) throw pas.SysUtils.Exception.$create("Create$1",["cssclasses is not an array"]);
        A = rtl.getObject(V);
        CSSClasses = pas.OTUtils.JSArrayOfStr2Pas(A);
      };
      V = AMsg["cssstyle"];
      if (pas.JS.isDefined(V)) {
        CSSStyle = "" + V;
      };
      OTC = $mod.ObjFactory.GetObjType(OTType);
      if (OTC === null) {
        this.AppendMsg(2,'ObjTerm: Received command "append" with invalid type "' + OTType + '" ("' + JSON.stringify(AMsg) + '")');
        this.SendMessage(this.CreateMsgCmdStatus(CmdId,"error",'Command "append" with invalid type "' + OTType + '"'));
        return;
      };
      OT = OTC.CreateFromCmdAppend(this,UniqueID,AMsg);
      OT.FCSSClasses = rtl.arrayRef(CSSClasses);
      OT.FCSSStyle = CSSStyle;
      this.AppendObj(OT);
      this.SendMessage(this.CreateMsgCmdStatus(CmdId,"ok",""));
    };
    this.HandleCmdObjCmd = function (AMsg) {
      var CmdId = "";
      var UniqueID = "";
      var OT = null;
      CmdId = "" + AMsg["cmdid"];
      UniqueID = "" + AMsg["uniqueid"];
      OT = this.FindObj(UniqueID);
      if (OT === null) {
        this.AppendMsg(2,'ObjTerm: Received command "objcmd" but couldn\'t find unique ID="' + UniqueID + '" ("' + JSON.stringify(AMsg) + '")');
        this.SendMessage(this.CreateMsgCmdStatus(CmdId,"error",'Command "objcmd" but couldn\'t find unique ID="' + UniqueID + '"'));
        return;
      };
      try {
        OT.ObjCmd(AMsg);
      } catch ($e) {
        if (pas.SysUtils.Exception.isPrototypeOf($e)) {
          var E = $e;
          this.AppendMsg(2,'ObjTerm: Received command "objcmd" and its execution resulted in the error: ' + E.fMessage);
          this.SendMessage(this.CreateMsgCmdStatus(CmdId,"error",'Command "objcmd" execution resulted in the error: ' + E.fMessage));
        } else throw $e
      };
    };
    this.CreateMessage = function (AMsgType) {
      var Result = null;
      Result = new Object();
      Result["msgtype"] = AMsgType;
      return Result;
    };
    this.CreateMsgHello = function () {
      var Result = null;
      Result = this.CreateMessage("hello");
      Result["version"] = "myversion";
      Result["features"] = Array.of("myfeature1","myfeature2");
      return Result;
    };
    this.CreateMsgCmdStatus = function (ACmdId, AStatus, AMessage) {
      var Result = null;
      Result = this.CreateMessage("cmdstatus");
      Result["cmdid"] = ACmdId;
      Result["status"] = AStatus;
      Result["message"] = AMessage;
      return Result;
    };
    this.CreateMsgInput = function (AText) {
      var Result = null;
      Result = this.CreateMessage("input");
      Result["text"] = AText;
      return Result;
    };
    this.SendMessage = function (AMsg) {
      this.FWS.send(JSON.stringify(AMsg));
    };
    this.Connect = function (AUrl) {
      this.FWSConnecting = true;
      this.FWS = new WebSocket(AUrl);
      this.FWS.onopen = rtl.createSafeCallback(this,"OnWSOpen");
      this.FWS.onerror = rtl.createSafeCallback(this,"OnWSError");
      this.FWS.onclose = rtl.createSafeCallback(this,"OnWSClose");
      this.FWS.onmessage = rtl.createSafeCallback(this,"OnWSMessage");
      this.FWSURL.disabled = true;
    };
    this.Disconnect = function () {
      this.FWSClosing = true;
      this.FWS.close();
    };
    this.OnWSMessage = function (Event) {
      var Result = false;
      this.HandleMessage(rtl.getObject(JSON.parse("" + Event.data)));
      Result = true;
      return Result;
    };
    this.OnWSClose = function (Event) {
      var Result = false;
      var CE = null;
      if (!rtl.isExt(Event,CloseEvent)) {
        this.AppendMsg(3,"ObjTerm: OnWSClose for " + this.FWS.url + " was called with an event which is not a CloseEvent: Type=" + Event.type + ", Target.ID=" + Event.target.id);
        return true;
      };
      CE = rtl.asExt(Event,CloseEvent);
      if (this.FWSConnecting) {
        this.FWSConnecting = false;
        this.AppendMsg(2,"ObjTerm: Could not connect to " + this.FWS.url + ". No additional information is available");
        if (this.FWSClosing) this.AppendMsg(3,"ObjTerm: FWSConnecting = true and FWSClosing = true at the same time");
      } else {
        if (CE.wasClean) {
          if (this.FWSClosing) {
            this.AppendMsg(0,"ObjTerm: Closed connection to " + this.FWS.url)}
           else this.AppendMsg(0,"ObjTerm: The client " + this.FWS.url + " closed the connection");
        } else {
          if (this.FWSClosing) {
            this.AppendMsg(2,"ObjTerm: Closed connection to " + this.FWS.url + " not clean. code = " + pas.SysUtils.IntToStr(CE.code) + ", reason = " + CE.reason)}
           else this.AppendMsg(2,"ObjTerm: The client " + this.FWS.url + " closed the connection not clean. code = " + pas.SysUtils.IntToStr(CE.code) + ", reason = " + CE.reason);
        };
      };
      this.FWSClosing = false;
      this.FWSURL.disabled = false;
      this.FWSConn.checked = false;
      this.InputDisable();
      Result = true;
      return Result;
    };
    this.OnWSError = function (Event) {
      var Result = false;
      this.FWSURL.disabled = false;
      this.FWSConn.checked = false;
      if (this.FWSConnecting) {}
      else {
        this.AppendMsg(2,"ObjTerm: An error occured with the WebSocket connection to " + this.FWS.url + ". No additional information is available.");
      };
      Result = true;
      return Result;
    };
    this.OnWSOpen = function (Event) {
      var Result = false;
      this.FWSConn.checked = true;
      this.FWSConnecting = false;
      this.AppendMsg(0,"ObjTerm: Established connection to " + this.FWS.url);
      this.SendMessage(this.CreateMsgHello());
      Result = true;
      return Result;
    };
  });
  rtl.createClass(this,"TOTObjFactory",pas.System.TObject,function () {
  });
  this.ObjFactory = null;
  $mod.$implcode = function () {
    $impl.CMsgSeverityCSSClass = ["otmsgnote","otmsgwarning","otmsgerror","otmsgfatal"];
    rtl.createClass($impl,"TOTObjFactoryImpl",$mod.TOTObjFactory,function () {
      this.$init = function () {
        $mod.TOTObjFactory.$init.call(this);
        this.FMap = null;
      };
      this.$final = function () {
        this.FMap = undefined;
        $mod.TOTObjFactory.$final.call(this);
      };
      this.Create$1 = function () {
        pas.System.TObject.$create("Create");
        this.FMap = pas["Generics.Collections"].TDictionary$G2.$create("Create$1",[0]);
        return this;
      };
      this.RegisterObjType = function (AObjType, AObjClass) {
        this.FMap.Add(AObjType,AObjClass);
      };
      this.GetObjType = function (AObjType) {
        var Result = null;
        if (!this.FMap.TryGetValue(AObjType,{get: function () {
            return Result;
          }, set: function (v) {
            Result = v;
          }})) Result = null;
        return Result;
      };
    });
  };
  $mod.$init = function () {
    $mod.ObjFactory = $impl.TOTObjFactoryImpl.$create("Create$1");
  };
},["OTOBase"]);
rtl.module("OTOBase",["System","JS","Classes","SysUtils","Web","Dygraph","OTObjTerm","OTUtils"],function () {
  "use strict";
  var $mod = this;
  rtl.createClass(this,"TOTOHTML",pas.OTObjTerm.TOTObject,function () {
    this.$init = function () {
      pas.OTObjTerm.TOTObject.$init.call(this);
      this.FHTML = "";
    };
    this.Create$2 = function (AObjTerm, AUniqueID, AHTML) {
      pas.OTObjTerm.TOTObject.Create$1.call(this,AObjTerm,AUniqueID);
      this.FHTML = AHTML;
      return this;
    };
    this.SetElementAndFill = function (AElement) {
      pas.OTObjTerm.TOTObject.SetElementAndFill.call(this,AElement);
      this.FElement.innerHTML = this.FHTML;
    };
    this.GetOTType = function () {
      var Result = "";
      Result = "otohtml";
      return Result;
    };
    this.CreateFromCmdAppend = function (AObjTerm, AUniqueID, AMsg) {
      var Result = null;
      Result = $mod.TOTOHTML.$create("Create$2",[AObjTerm,AUniqueID,"" + AMsg["html"]]);
      return Result;
    };
  });
  rtl.createClass(this,"TOTOString",this.TOTOHTML,function () {
    this.Create$3 = function (AObjTerm, AUniqueID, ASt) {
      $mod.TOTOHTML.Create$2.call(this,AObjTerm,AUniqueID,this.$class.Escape(ASt));
      return this;
    };
    this.Append = function (ASt) {
      this.FElement.innerHTML = this.FElement.innerHTML + this.$class.Escape(ASt);
    };
    this.GetOTType = function () {
      var Result = "";
      Result = "otostring";
      return Result;
    };
    this.ObjCmd = function (AMsg) {
      var CmdId = "";
      var SubCmd = "";
      var St = "";
      var Status = null;
      CmdId = "" + AMsg["cmdid"];
      SubCmd = "" + AMsg["subcmd"];
      if (SubCmd === "append") {
        St = "" + AMsg["st"];
        this.Append(St);
        this.FObjTerm.SendMessage(this.FObjTerm.CreateMsgCmdStatus(CmdId,"ok",""));
      } else if (SubCmd === "getlength") {
        Status = this.FObjTerm.CreateMsgCmdStatus(CmdId,"ok","");
        Status["length"] = this.FElement.innerHTML.length;
        this.FObjTerm.SendMessage(Status);
      } else {
        throw pas.SysUtils.Exception.$create("Create$1",['Invalid subcmd = "' + SubCmd + '"']);
      };
    };
    this.Escape = function (ASt) {
      var Result = "";
      ASt = pas.SysUtils.StringReplace(ASt,"<","&lt;",rtl.createSet(0));
      ASt = pas.SysUtils.StringReplace(ASt,">","&gt;",rtl.createSet(0));
      ASt = pas.SysUtils.StringReplace(ASt," ","&nbsp;",rtl.createSet(0));
      ASt = pas.SysUtils.StringReplace(ASt,"\r\n","<br>",rtl.createSet(0));
      ASt = pas.SysUtils.StringReplace(ASt,"\n","<br>",rtl.createSet(0));
      ASt = pas.SysUtils.StringReplace(ASt,"\r","<br>",rtl.createSet(0));
      Result = ASt;
      return Result;
    };
    this.CreateFromCmdAppend = function (AObjTerm, AUniqueID, AMsg) {
      var Result = null;
      Result = $mod.TOTOString.$create("Create$3",[AObjTerm,AUniqueID,"" + AMsg["st"]]);
      return Result;
    };
  });
  rtl.createClass(this,"TOTODygraph",pas.OTObjTerm.TOTObject,function () {
    this.$init = function () {
      pas.OTObjTerm.TOTObject.$init.call(this);
      this.FWidth = 0;
      this.FHeight = 0;
      this.FData = null;
      this.FOptions = undefined;
      this.FDiv = null;
      this.FGraph = null;
    };
    this.$final = function () {
      this.FData = undefined;
      this.FDiv = undefined;
      this.FGraph = undefined;
      pas.OTObjTerm.TOTObject.$final.call(this);
    };
    this.Create$2 = function (AObjTerm, AUniqueID, AWidth, AHeight, AData, AOptions) {
      pas.OTObjTerm.TOTObject.Create$1.call(this,AObjTerm,AUniqueID);
      this.FWidth = AWidth;
      this.FHeight = AHeight;
      this.FData = AData;
      this.FOptions = AOptions;
      return this;
    };
    this.SetElementAndFill = function (AElement) {
      pas.OTObjTerm.TOTObject.SetElementAndFill.call(this,AElement);
      this.FDiv = document.createElement("div");
      this.FElement.appendChild(this.FDiv);
      this.FDiv.style.setProperty("width",pas.SysUtils.IntToStr(this.FWidth) + "px");
      this.FDiv.style.setProperty("height",pas.SysUtils.IntToStr(this.FHeight) + "px");
      this.FDiv.style.setProperty("margin","5px");
      this.FDiv.style.setProperty("padding","5px");
      this.FDiv.style.setProperty("color","#000000");
      this.FDiv.style.setProperty("background","#FFFFFF");
      this.FGraph = new Dygraph(this.FDiv,this.FData,this.FOptions);
    };
    this.AppendPoints = function (AData, ABlockRedraw) {
      var Point = undefined;
      for (var $in = AData, $l = 0, $end = rtl.length($in) - 1; $l <= $end; $l++) {
        Point = $in[$l];
        this.FGraph.file_.push(rtl.getObject(Point));
      };
      if (!ABlockRedraw) this.FGraph.start_();
    };
    this.GetOTType = function () {
      var Result = "";
      Result = "otodygraph";
      return Result;
    };
    this.ObjCmd = function (AMsg) {
      var CmdId = "";
      var SubCmd = "";
      var Data = null;
      var BlockRedraw = false;
      CmdId = "" + AMsg["cmdid"];
      SubCmd = "" + AMsg["subcmd"];
      if (SubCmd === "appendpoints") {
        Data = rtl.getObject(AMsg["data"]);
        BlockRedraw = !(AMsg["blockredraw"] == false);
        this.AppendPoints(Data,BlockRedraw);
        this.FObjTerm.SendMessage(this.FObjTerm.CreateMsgCmdStatus(CmdId,"ok",""));
      } else {
        throw pas.SysUtils.Exception.$create("Create$1",['Invalid subcmd = "' + SubCmd + '"']);
      };
    };
    this.CreateFromCmdAppend = function (AObjTerm, AUniqueID, AMsg) {
      var Result = null;
      Result = $mod.TOTODygraph.$create("Create$2",[AObjTerm,AUniqueID,rtl.trunc(AMsg["width"]),rtl.trunc(AMsg["height"]),rtl.getObject(AMsg["data"]),AMsg["options"]]);
      return Result;
    };
  });
  $mod.$init = function () {
    if (!(pas.OTObjTerm.ObjFactory != null)) throw pas.SysUtils.Exception.$create("Create$1",["ObjFactory is not assigned. Did you use OTObjTerm before OTOBase"]);
    pas.OTObjTerm.ObjFactory.RegisterObjType("otohtml",$mod.TOTOHTML);
    pas.OTObjTerm.ObjFactory.RegisterObjType("otostring",$mod.TOTOString);
    pas.OTObjTerm.ObjFactory.RegisterObjType("otodygraph",$mod.TOTODygraph);
  };
});
rtl.module("program",["System","OTOBase","OTObjTerm"],function () {
  "use strict";
  var $mod = this;
  this.OT = null;
  $mod.$main = function () {
    $mod.OT = pas.OTObjTerm.TObjTerm.$create("Create$1");
    $mod.OT.AppendOTHTML("Welcome to <b>ObjTerm<\/b>!");
  };
});
//# sourceMappingURL=objterm.js.map
