// Progressive enhancement
(function () {
    "use strict";
    window.addEventListener("load", function () {
        console.log("Initialising reports");
        document.querySelectorAll("table.futu-report").forEach(initFutuReport);
    });

    function getColumn(td) {
        return _.indexOf(td.parentElement.children, td);
    }

    function initFutuReport(table) {
        var ths = table.querySelectorAll("thead th");
        var trs = table.querySelectorAll("tbody tr");
        var tds = table.querySelectorAll("tbody td");
        var tbody = table.querySelector("tbody");

        // The enclosing foundation row
        var row = table;
        while (row.className !== "row" && row !== null) {
            row = row.parentElement;
        }
        if (!row) {
            row = table;
        }

        // Model signals
        var filters = _.map(ths, function () {
            return menrva.source([], _.isEqual);
        });

        var allFilter = menrva.combine.apply(null, filters.concat([function () {
            return _.slice(arguments);
        }]));

        // Filtering logic
        allFilter.onValue(function (xss) {
            function visible(tr) {
                return _.every(xss, function (xs, i) {
                    if (xs.length === 0) { return true; }

                    var value = tr.children[i].innerText;
                    return xs.indexOf(value) !== -1;
                });
            }

            var visibilities = _.map(trs, visible);
            _.zipWith(trs, visibilities, function (tr, visible) {
                tr.style.display = visible ? "" : "none";
            });
        });

        // control panel
        var controls = [
            dom("a", {
                className: "futu-unfilter",
                innerText: "Show all entries" ,
                click: unfilter
            })
        ];

        _.each(ths, function (th, column) {
            var name = th.innerText;

            var values = _.map(trs, function (tr) {
                return tr.children[column].innerText;
            });

            var uniqValues = _.sortedUniq(values.sort());

            if (uniqValues.length <= 10) {

                var el = row12([
                    dom("label", [
                        name,
                        dom("select", {
                            multiple: "multiple",
                            change: function (ev) {
                                var select = ev.target || ev.srcElement;
                                var options = _.chain(select.options)
                                    .filter((o) => o.selected)
                                    .map(o => o.value)
                                    .value();

                                menrva.transaction()
                                    .set(filters[column], options)
                                    .commit();
                            }
                        }, _.map(uniqValues, function (v) {
                            return dom("option", {
                                value: v
                            }, [ v ]);
                        }))
                    ])
                ]);

                controls.push(el);
            }
        });

        var controlWrapper = row12([dom("div", { className: "callout" }, controls)]);

        // insert control panel
        row.parentElement.insertBefore(controlWrapper, row);

        // Unfilter
        function unfilter() {
            var tx = menrva.transaction();
            _.each(filters, function (f) {
                tx.set(f, []);
            });
            tx.commit();
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
                    menrva.transaction()
                        .set(filters[column], [value])
                        .commit();
                }
            });
        });
    }

    // Utilities
    var eventNames = [ "click", "change" ];

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
            if (_.isString(child)) {
                el.appendChild(domText(child));
            } else {
                el.appendChild(child);
            }
        });

        return el;
    }

    function domText(t) {
        return document.createTextNode(t);
    }

    function row12(children) {
        return dom("div", { className: "row" }, [
            dom("div", { className: "columns large-12" }, children)
        ]);
    }
}());
