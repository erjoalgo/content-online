function createElementWithProperties(tag, textContent, props, children) {
    props = props || {};
    props.textContent = props.textContent || textContent;

    var elm = document.createElement(tag);
    for (var key in props) {
        if (props.hasOwnProperty(key))    {
            var val = props[key];
            var path = key.split(".");
            var last = path.pop();
            var nested = path.reduce(function(cum, a) {
                return cum[a];
            }, elm);
            nested[last] = val;
        }
    }
    for (var i = 0; i<(children || []).length; i++) {
        var child = children[i];
        if (typeof(child) == "string") {
            elm.insertAdjacentText('beforeend', child);
        } else  {
            elm.appendChild(child);
        }
    }
    return elm;
}

var elm = createElementWithProperties;

function deleteButton ( url, text ) {
    text = text || "DELETE";
    var butt = elm("button", text);
    var cell = elm("div", null, null, [butt]);
    butt.onclick = (function(){
        var _div = cell;
        var _butt = butt;
        var _actionUrl = url;
        return function(){
            var xhr = new XMLHttpRequest();
            xhr.open('GET', _actionUrl, true);
            xhr.setRequestHeader('Content-type', 'application/json');
            xhr.onreadystatechange = function() {
                if(xhr.readyState == 4){
                    var resp = xhr.responseText;
                    var isOk = Math.floor(xhr.status/100) == 2;
                    var node = elm("div");
                    node.innerHTML = resp;
                    // xhr.status + " " + resp,
                    var newText = elm("font",
                                      xhr.status,
                                      {color: isOk? "green": "red"},
                                      [node]);
                    _div.removeChild(_div.firstChild);
                    _div.appendChild(newText);
                    // if (!isOk) {alert(_actionUrl+" failed: "+resp);}
                }
            };
            _butt.disabled = true;
            xhr.send();
        };
    }());
    return cell;
}

var loadingGifUrl = "/www/img/loading-small.gif";
function lazyElm ( value ) {
    var loadUrl = value.url;
    var tmpImageSrc = value.imgSrc || loadingGifUrl;
    var img = elm("img", null, {src: tmpImageSrc});
    var div = elm("div", null, null, [img]);
    img.load = (function(){
        var _div = div;
        var _loadUrl = loadUrl;
        return function(){
            var xhr = new XMLHttpRequest();
            xhr.open('GET', _loadUrl, true);
            xhr.onreadystatechange = function() {
                if(xhr.readyState == 4){
                    var resp = xhr.responseText;
                    var isOk = Math.floor(xhr.status/ 100) == 2;
                    var node = elm("div");
                    node.innerHTML = resp;
                    var newText = elm("font",
                                      null,
                                      {color: isOk? "black": "red"},
                                      [node]);
                    _div.removeChild(_div.firstChild);
                    _div.appendChild(newText);
                }
            };
            xhr.send();
        };
    }());
    img.load();
    return div;
}

function tableCell ( value ) {
    var cell;
    if (value === null) {
        return "";
    }else if (-1 != ["number", "string"].indexOf(typeof(value))) {
        cell = elm("div", value);
    } else if (value.type == "link") {
        cell = elm("a", value.text,
                   {href: value.url});
    } else if (value.type == "button/delete") {
        cell = elm("div", null, null, [deleteButton(value.url, value.text)]);
        }
    else if (value.type == "lazy-elm") {
        cell = lazyElm(value);
    } else  {
        throw "unknown item value type: " + value.type;
        // cell = elm("div");
    }
    return cell;
}

function fetchData ( tableDataUrl ) {
    return new Promise(function(resolve, reject) {
        var xhr = new XMLHttpRequest();
        xhr.open('GET', tableDataUrl, true);
        xhr.setRequestHeader('Accept', 'application/json');
        xhr.onreadystatechange = function() {
            if(xhr.readyState == 4){
                if ( xhr.status != 200) {
                    var err = "GET "+ tableDataUrl+" failed: "+xhr.status +
                        " "+xhr.responseText;
                    console.error(err);
                    reject(err);
                }else{
                    var data = JSON.parse(xhr.responseText);
                    resolve(data);
                }
            }
        };
        xhr.send(null);
    });
}

function handleError(message) {
    console.error(new Error().stack);
    var msg = "error: " + message;
    console.error(msg);
    alert(msg);
    throw new Error(message);
}

function renderTable ( tableDataUrl, div ) {
    return fetchData(tableDataUrl).
        then(function(data){
            var headers = data.headers;
            var items = data.items;
            headers.unshift("#");

            var table = elm("table", null, {border: 1, cellpadding: 4});
            table.appendChild(elm("tr", null, null,
                                  headers.map(function(header){
                                      return elm("td", null, null,
                                                 [elm("b", header)]);
                                  })));

            for (var i = 0; i<items.length; i++)
            {
                var item = items[i];
                var tr = elm("tr", null, null, [elm("td", i)]);
                for (var ii = 1; ii<headers.length; ii++) {
                    var header = headers[ii];
                    var value = item[header];
                    if (value === undefined) {
                        console.warn("missing header: "+header);
                    } else  {
                        var cell = tableCell(value);
                        tr.appendChild(elm("td", null, null, [cell]));
                    }
                }
                table.append(tr);
            }
            div.appendChild(table);
        });
}

window.onload = function(){
    var tableDataUrl = window.location.href.slice().replace(/.html/, "");
    var div = elm("div");
    document.body.appendChild(div);
    renderTable(tableDataUrl, div).
        catch(handleError);
};
