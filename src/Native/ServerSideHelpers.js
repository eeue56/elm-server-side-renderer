var _eeue56$elm_server_side_renderer$Native_ServerSideHelpers = function() {

var STYLE_KEY = 'STYLE';
var EVENT_KEY = 'EVENT';
var ATTR_KEY = 'ATTR';
var ATTR_NS_KEY = 'ATTR_NS';

function copy(o) {
  var _out, v, _key;
  _out = Array.isArray(o) ? [] : {};
  for (_key in o) {
    v = o[_key];
    _out[_key] = (typeof v === "object") ? copy(v) : v;
  }
  return _out;
}

function organizeFacts(fact, facts)
{
    var entry = fact;
    var key = entry.key;

    if (key === ATTR_KEY || key === ATTR_NS_KEY || key === EVENT_KEY)
    {
        var subFacts = facts[key] || {};
        subFacts[entry.realKey] = entry.value;
        facts[key] = subFacts;
    }
    else if (key === STYLE_KEY)
    {
        var styles = facts[key] || {};
        var styleList = entry.value;
        while (styleList.ctor !== '[]')
        {
            var style = styleList._0;
            styles[style._0] = style._1;
            styleList = styleList._1;
        }
        facts[key] = styles;
    }
    else if (key === 'namespace')
    {
        namespace = entry.value;
    }
    else
    {
        facts[key] = entry.value;
    }

    return facts;
}



function addAttribute(attribute, node){
    var newNode = copy(node);

    organizeFacts(attribute, newNode.facts);

    return newNode;
}

function replaceChildren(nodeString){
    var node = null;

    try {
        node = JSON.parse(nodeString)
    } catch (e){
        return nodeString;
    }

    var children = node.children;
    if (typeof children === "undefined" || children === null) return nodeString;

    var newChildren = [];

    Object.keys(children).forEach(function(key){
        var item = children[key];
        newChildren.push(item);
    });

    node.children = newChildren;

    return JSON.stringify(newChildren);
}

function triggerEvent(eventName, value, node){
    var facts = node.facts;
    var events = facts['EVENT'];

    if (typeof events === "undefined" || events === null
        || typeof events[eventName] === "undefined" || events[eventName] === null){
        _elm_lang$core$Result$Err("Event " + eventName + " not found");
    }

    return A2(_elm_lang$core$Native_Json.run, events[eventName].decoder, value)
}

return {
    replaceChildren: replaceChildren,
    stringify: JSON.stringify,
    addAttribute: F2(addAttribute),
    triggerEvent: F3(triggerEvent)
};

}();
