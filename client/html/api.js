function sendRequest() {
    var request=new XMLHttpRequest();
    request.onreadystatechange=function() {
        if (request.readyState == 4) {
            if (request.status==200 || window.location.href.indexOf("http") == -1) {
                var jsondata=eval("("+request.responseText+")");
                for(var key in jsondata) {
                    console.log("key=" + key + ", value=" + jsondata[key]);
                };
    //            var output='<ul>'
    //            for (var i=0; i<rssentries.length; i++) {
    //                output+='<li>'
    //                output+='<a href="'+rssentries[i].link+'">'
    //                output+=rssentries[i].title+'</a>'
    //                output+='</li>'
    //            }
    //            output+='</ul>'
    //            document.getElementById("result").innerHTML=output
            } else {
                alert("An error has occured making the request")
            }
        }
    }

    var parameters = {
        "url": "https://whatever.test.test8"
    };
    parameters = JSON.stringify(parameters);
    console.log("parameters=" + parameters);

    request.open("POST", "http://127.0.0.1:8000/addUrl", true);
    request.send(parameters);
    return true;
};
