var _eeue56$elm_server_side_renderer$Native_Helpers = function() {

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

return {
    replaceChildren: replaceChildren,
    stringify: JSON.stringify
};

}();
