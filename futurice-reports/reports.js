// Progressive enhancement
(function () {
    "use strict";
    window.addEventListener("load", function () {
        console.log("Initialising reports"); 
        document.querySelectorAll("table.futu-report").forEach(initFutuReport);
    });

    function getColumn(td) {
        return Array.prototype.slice.call(td.parentElement.children).indexOf(td);
    }

    function initFutuReport(table) {
        var ths = table.querySelectorAll("thead th");
        var trs = table.querySelectorAll("tbody tr");
        var tds = table.querySelectorAll("tbody td");
        var tbody = table.querySelector("tbody");

        // because looking up information in DOM is slow
        // (when interleaved with style changes)
        var values = _.map(trs, function (tr) {
            return _.map(tr.children, function (td) {
                return td.innerText;
            });
        });

        // Unfilter
        function unfilter() {
            console.info("Unfilter");
            trs.forEach(function (tr) {
                tr.style.display = "";
            });
        }

        // we add "show all" to column
        // TODO: there should be better place to add this?
        function addUnfilter(column) {
            var th = ths[column];

            if (th.querySelectorAll(".futu-unfilter").length === 0) {
                th.appendChild(domText(" "));
                
                var el = dom("a", {
                    className: "futu-unfilter",
                    innerText: "show all" ,
                    click: unfilter
                });

                th.appendChild(el);
            }
        }

        // sort by column
        ths.forEach(function (th) {
            var column = getColumn(th);
            var text = th.innerText;
          
            var el = dom("a", {
                href: "#",
                className: "futu-sort",
                innerText: text,
                click: function () {
                    console.log("sort based on column", column); 

                    // sort rows
                    var sortedTrs = _.sortBy(trs, function (tr) {
                        return tr.children[column].innerText;
                    });

                    // remove unsorted rows
                    while (tbody.lastChild) {
                        tbody.removeChild(tbody.lastChild);
                    }

                    // insert sorted rows
                    sortedTrs.forEach(function (tr) {
                        tbody.appendChild(tr);
                    });
                }
            });

            th.innerText = "";
            th.appendChild(el);
        });

        // filter by value
        tds.forEach(function (td) {
            var column = getColumn(td);
            var hasLink = td.querySelectorAll("a").length !== 0;

            if (!hasLink) {
                td.style.cursort = "pointer";
            }
            var value = td.innerText;

            td.addEventListener("click", function () {
                // atm only values with no links
                if (!hasLink) {
                    addUnfilter(column);
                    trs.forEach(function (tr, i) {
                        var value2 = values[i][column];
                        if (value !== value2) {
                            tr.style.display = "none";
                        }
                    });
                }
            });
        });
    }

    // Utilities
    var eventNames = [ "click" ];
    
    function dom(elName, args, children) {
        if (_.isArray(args)) {
            children = args;
            args = {};
        }

        children = children || [];
        args = args || {};

        var el = document.createElement(elName);

        _.forEach(args, function (value, key) {
            if (eventNames.indexOf(key) === -1) {
                el[key] = value;
            } else {
                el.addEventListener(key, value);
            }
        });

        children.forEach(function (child) {
            el.appendChild(child);
        });

        return el;
    }

    function domText(t) {
        return document.createTextNode(t);
    }
}());
