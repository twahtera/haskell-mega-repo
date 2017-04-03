window.addEventListener("load", function () {
    "use strict";
    console.info("Reports magic!");

    // Main function, called at the very end.
    // We wrap stuff here, so we can initialise utilities below.
    function main() {
        console.info("Reports main");

        var reportEl = $(".futu-report-wrapper");
        if (!reportEl) {
            console.error("There are no report on this page");
            return;
        }

        var dataEl = $(".futu-report-data", reportEl);
        var data;
        try {
            data = JSON.parse(dataEl.innerText);
        } catch (e) {
            console.error("There are no report data");
            return;
        }

        // We have data!
        console.debug(data);
        assert(data.columns.length === data.types.length, "length columns == length types");

        // Collect elements
        var tableEl       = $("table.futu-report", reportEl);
        var tableHeadEl   = $("table.futu-report thead", reportEl);
        var tableBodyEl   = $("table.futu-report tbody", reportEl);
        var toggleButton  = $("button.futu-report-toggle", reportEl);
        var toggleableEl  = $("div.futu-report-toggleable-controls", reportEl);
        var toggleableEl2 = $("div.futu-report-toggleable-controls-2", reportEl);
        var quickControls = $("tr.futu-report-quick-controls", reportEl);
        var queryStringEl = $("pre.futu-report-query-str", reportEl);
        var applyButton   = $("button.futu-report-apply", reportEl);
        var resetButton   = $("button.futu-report-reset", reportEl);

        var elements = [
            tableEl, tableHeadEl, tableBodyEl,
            toggleButton, toggleableEl, toggleableEl2,
            quickControls,
            queryStringEl,
            applyButton, resetButton,
        ];

        if (!_.every(elements)) {
            console.error("Some element is missing", elements);
            return;
        }

        // toggleable control.
        // We don't bother to model this with menrva
        toggleableEl.style.display = toggleableEl2.style.display = "none";
        toggleButton.innerText = "Show controls";
        toggleButton.addEventListener("click", function () {
            var display = toggleableEl.style.display;
            display = display === "none" ? "" : "none";
            var text = display === "none" ? "Show controls" : "Hide controls";
            toggleableEl.style.display = display;
            toggleableEl2.style.display = display;
            toggleButton.innerText = text;
        });

        /* CONTROLS */

        var VALID_AGGREGATES = [
            "first",
            "sum", "avg",
            "min", "max",
            "count", "countDistinct",
            "collect", "collectDistinct",
        ];

        // todo parse query string
        var queryObject = parseQueryString(window.location.search);
        console.info("Query", queryObject);

        var START_GROUPBY = _.chain((queryObject["group-by"] || "").split(/,/))
            .map(function (column) {
                var colIdx = data.columns.indexOf(column);
                if (colIdx === -1) {
                    return null;
                } else {
                    return colIdx;
                }
            })
            .filter(function (x) { return x !== null; })
            .sort()
            .uniq()
            .value();

        var START_AGGREGATES = _.map(data.columns, function (column) {
            var aggr = queryObject[column + "-aggr"];
            if (VALID_AGGREGATES.indexOf(aggr) !== -1) {
                return aggr;
            } else {
                return "first";
            }
        });

        var START_FILTERBY = _.map(data.columns, function (column) {
            try {
                var vals = queryObject[column + "-filter"];
                if (!vals) return [];
                vals = JSON.parse(vals);
                if (!Array.isArray(vals)) { vals = [vals]; }
                vals.sort();
                return vals;
            } catch (e) {
                console.error(e);
                return [];
            }
        });

        var START_ORDERBY = _.chain((queryObject["order-by"] || "").split(/,/))
            .map(function (sortSpec) {
                var m = sortSpec.match(/^(.*)-(asc|desc)$/);
                if (!m) return null;
                var column = m[1];
                var order = m[2];

                var colIdx = data.columns.indexOf(column);
                if (colIdx === -1) {
                    return null;
                } else {
                    return { colIdx: colIdx, order: order };
                }
            })
            .filter(function (x) { return x !== null; })
            .sort()
            .uniqBy(function (x) { return x.colIdx; })
            .value();


        var START_SETTINGS = {
            orderBy:    START_ORDERBY,
            aggregates: START_AGGREGATES,
            groupBy:    START_GROUPBY,
            filterBy:   START_FILTERBY,
            pending:    [], // pending updates
        };

        var settings$ = menrva.source(START_SETTINGS, _.isEqual);

        /* Events */
        function addEventToTransaction(transaction, e) {
            return transaction.modify(settings$, function (settings) {
                return _.merge({}, settings, {
                    pending: settings.pending.concat([e]),
                });
            });
        }

        function applyEventsInTransaction(transaction) {
            return transaction.modify(settings$, applyEvents);
        }

        function applyEvents(settings) {
            settings = _.cloneDeep(settings);
            _.each(settings.pending, function (e) {
                switch (e.type) {
                    case "sort":
                        settings.orderBy = normaliseOrderBy(
                            [ { colIdx: e.colIdx, order: e.order } ].concat(settings.orderBy));
                        break;

                    case "group-toggle":
                        settings.groupBy = (function () {
                            var groupBy = settings.groupBy;
                            var colIdx = e.colIdx;
                            if (_.indexOf(groupBy, colIdx) === -1) {
                                return groupBy.concat([colIdx]).sort();
                            } else {
                                return groupBy.filter(function (x) { return x !== colIdx; });
                            }
                        }());
                        break;

                    case "group-set":
                        settings.groupBy = (function () {
                            var groupBy = settings.groupBy;
                            var colIdx = e.colIdx;
                            if (e.checked) {
                                return _.uniq(groupBy.concat([colIdx]).sort());
                            } else {
                                return groupBy.filter(function (x) { return x !== colIdx; });
                            }
                        }());
                        break;

                    case "aggregate":
                        settings.aggregates = _.map(settings.aggregates, function (val, idx) {
                            return idx === e.colIdx ? e.aggr : val;
                        });
                        break;

                    case "filterBy":
                        settings.filterBy[e.colIdx] = e.values;
                        break;

                    default:
                        assert(false, "Unknown event type: " + e.type);
                }
            });
            settings.pending = [];
            return settings;
        }

        function sortEvent(colIdx, order) {
            return {
                type:  "sort",
                colIdx: colIdx,
                order:  order,
            };
        }

        function toggleGroupEvent(colIdx) {
            return {
                type:   "group-toggle",
                colIdx: colIdx,
            };
        }

        function setGroupEvent(colIdx, checked) {
            return {
                type:   "group-set",
                colIdx:  colIdx,
                checked: checked,
            };
        }

        function aggregateEvent(colIdx, aggr) {
            assert(_.indexOf(VALID_AGGREGATES, aggr) !== -1, "Invalid aggregate: " + aggr);
            return {
                type:   "aggregate",
                colIdx: colIdx,
                aggr:   aggr,
            };
        }

        function filterByEvent(colIdx, values) {
            return {
                type:   "filterBy",
                colIdx: colIdx,
                values: values,
            };
        }

        /* Auxiliary */

        function normaliseOrderBy(so) {
            return _.uniqBy(so, function (x) { return x.colIdx; });
        }

        /* Transactions */

        function sortAscending(colIdx, applyEvent) {
            var t = menrva.transaction();
            addEventToTransaction(t, sortEvent(colIdx, "asc"));
            if (applyEvent) { applyEventsInTransaction(t); }
            t.commit();
        }

        function sortDescending(colIdx, applyEvent) {
            var t = menrva.transaction();
            addEventToTransaction(t, sortEvent(colIdx, "desc"));
            if (applyEvent) { applyEventsInTransaction(t); }
            t.commit();
        }

        function toggleGroupBy(colIdx, applyEvent) {
            var t = menrva.transaction();
            addEventToTransaction(t, toggleGroupEvent(colIdx));
            if (applyEvent) { applyEventsInTransaction(t); }
            t.commit();
        }

        function setGroupBy(colIdx, checked, applyEvent) {
            var t = menrva.transaction();
            addEventToTransaction(t, setGroupEvent(colIdx, checked));
            if (applyEvent) { applyEventsInTransaction(t); }
            t.commit();
        }

        function setAggregate(colIdx, aggr, applyEvent) {
            var t = menrva.transaction();
            addEventToTransaction(t, aggregateEvent(colIdx, aggr));
            if (applyEvent) { applyEventsInTransaction(t); }
            t.commit();
        }

        function setFilterBy(colIdx, values, applyEvent) {
            var t = menrva.transaction();
            addEventToTransaction(t, filterByEvent(colIdx, values));
            if (applyEvent) { applyEventsInTransaction(t); }
            t.commit();
        }

        /* Settings debug */
        settings$.onValue(function (s) { console.log("Settings", s); });

        // history!
        // http://stackoverflow.com/questions/824349/modify-the-url-without-reloading-the-page
        /*

        // We have to careful:
        // - do not push first state
        // - do not push state after pop

        settings$.onValue(function (s) {
            console.info("pushing history", s);
            window.history.pushState(s, "ignored title", "?s=" +
                encodeURIComponent(JSON.stringify(s)));
        });

        window.addEventListener("popstate", function (e) {
            if (!e.state) return;
            var s = e.state;
            console.info("popped history", s);

            menrva.transaction()
                .set(orderBy, s.orderBy)
                .commit();
        });
        */

        /* Render code */
        settings$.onValue(function (settings) {
            // we apply events. so "SQL" shows what we are clicked.
            settings = applyEvents(settings);

            // select
            var fields = _.zipWith(data.columns, settings.aggregates, function (col, agg) {
                if (agg === "first") {
                    return col;
                } else {
                    return agg.toUpperCase() + "(" + col + ")";
                }
            });

            // where
            var filterBy = _.chain(settings.filterBy)
                .map(function (values, colIdx) {
                    if (values.length === 0) { return null; }
                    if (values.length === 1) { return data.columns[colIdx] + " = '" + values[0] + "'"; }
                    return data.columns[colIdx] + " IN (" +
                        values.map(function (x) { return "'" + x + "'"; }).join(", ") +
                        ")";
                })
                .compact()
                .value()
                .join(",\n      "); // align


            if (filterBy !== "") {
                filterBy = "WHERE " + filterBy;
            }

            // group by
            var groupBy = _.map(settings.groupBy, function (col) {
                return data.columns[col];
            }).join(", ");

            if (groupBy !== "") {
                groupBy = "GROUP BY " + groupBy;
            }

            // order by
            var orderBy = _.map(settings.orderBy, function (so) {
                return data.columns[so.colIdx] + " " + so.order.toUpperCase();
            }).join(", ");

            if (orderBy !== "") {
                orderBy = "ORDER BY " + orderBy;
            }

            // the query
            var query = [
                "SELECT " + fields.join(", "),
                "FROM '" + data.name + "'",
                filterBy, groupBy, orderBy,
            ].filter(function (x) { return x !== ""; }).join("\n");

            // assing to dom
            queryStringEl.innerText = query;
        });

        /* Table titles */

        var titleCells = $$("thead th", reportEl);
        assert(titleCells.length == data.columns.length, "Wrong amount of title cells");

        _.each(titleCells, function (th, colIdx) {
            settings$.map(function (settings) {
                return settings.aggregates[colIdx];
            }).onValue(function (aggr) {
                if (aggr === "first") {
                    th.innerText = data.columns[colIdx];
                } else {
                    th.innerText = aggr.toUpperCase() + "(" + data.columns[colIdx] + ")";
                }
            });
        });

        /* Rendering */

        var nonAppliedSettings$ = settings$.map(function (settings) {
            return _.pick(settings, ["groupBy", "orderBy", "aggregates", "filterBy"]);
        }, _.isEqual);

        nonAppliedSettings$.onValue(function (settings) {
            console.info("Rendering");
            var contents = _.map(data.data); // copy

            // filter
            _.each(settings.filterBy, function (values, colIdx) {
                if (values.length === 0) return;

                contents = contents.filter(function (row) {
                    return values.indexOf(row[colIdx]) !== -1;
                });
            });

            // group
            if (settings.groupBy.length !== 0) {
                var grouped = superGroupBy(contents, function (el) {
                    return _.map(settings.groupBy, function (colIdx) { return el[colIdx]; });
                });

                // console.debug(grouped);

                // apply aggregates
                contents = _.map(grouped, function (g) {
                    // we don't need key
                    var els = g[1];

                    // TODO: return first for all
                    return _.map(settings.aggregates, function (agg, colIdx) {
                        var cells = els.map(function (el) { return el[colIdx]; });
                        return aggregateFunctions[agg](cells);
                    });
                });
            }

            // apply sorts
            var orderBy = settings.orderBy;
            if (orderBy.length !== 0) {
                var sortIteratees = _.map(orderBy, function (so) {
                    return function (x) { return x[so.colIdx]; };
                });
                var orderBys = _.map(orderBy, "order");
                contents = _.orderBy(contents, sortIteratees, orderBys);
            }

            var rows = _.map(contents, function (entry) {
                return dom("tr", _.map(entry, function (cell, colIdx) {
                    return dom("td", [ renderCellContents(cell, data.types[colIdx]) ]); // to apply function
                }));
            });

            // insert into dom
            tableBodyEl.innerHTML = "";
            _.each(rows, function (row) {
                tableBodyEl.appendChild(row);
            });
        });

        /* Cell rendering */
        function renderCellContents(contents, type) {
            if (contents === null) return dom("i", "???");
            switch (type) {
                case "fumUser":
                    return dom("a", { href: data.params.fumPublicUrl + "fum/users/" + contents }, [ contents ]);
                case "hourDiff":
                    return "" + contents + " h";
                case "dayDiff":
                    return "" + contents + " days";
                default:
                    return "" + contents;
            }
        }

        /* Bind quick controls */
        assert(_.slice(quickControls.children).length === data.columns.length, "Wrong amount of quick controls");

        _.each(quickControls.children, function (qc, colIdx) {
            var links = $$("a", qc);
            _.each(links, function (link) {
                link.addEventListener("click", function () {
                    var action = link.dataset.futuReportLinkControl;
                    switch (action) {
                        case "sort-asc":      sortAscending(colIdx, true);  break;
                        case "sort-desc":     sortDescending(colIdx, true); break;
                        case "group-by":      toggleGroupBy(colIdx, true); break;
                        default:
                            if (VALID_AGGREGATES.indexOf(action) !== -1) {
                                setAggregate(colIdx, action, true);
                                break;
                            }
                            assert(false, "Unknown quick control: " + action);
                    }

                    // don't act as link
                    return false;
                });
            });
        });

        /* Bind slow controls */

        // Apply & reset
        resetButton.addEventListener("click", function () {
            menrva.transaction()
                .set(settings$, START_SETTINGS)
                .commit();
        });

        var hasPendingEvents$ = settings$.map(function (s) { return s.pending.length !== 0; });
        hasPendingEvents$.onValue(function (hasPendingEvents) {
            applyButton.disabled = !hasPendingEvents;
        });

        applyButton.addEventListener("click", function () {
            var t = menrva.transaction();
            applyEventsInTransaction(t);
            t.commit();
        });

        /* rest of the controls */
        var slowControls = $$("div.futu-report-control");
        assert(slowControls.length === data.columns.length, "Wrong amount of slow controls");
        _.each(slowControls, function (control, colIdx) {
            var aggregateEl = $("select.futu-report-aggregate", control);
            var groupByEl = $("input.futu-report-group-by", control);

            assert(_.every([aggregateEl, groupByEl]), "Some of slow controls missing");

            // Group By
            groupByEl.addEventListener("change", function () {
                setGroupBy(colIdx, groupByEl.checked);
            });

            settings$.map(function (settings) {
                return settings.groupBy.indexOf(colIdx) !== -1;
            }).onValue(function (checked) {
                groupByEl.checked = checked;
            });

            // Aggregate
            aggregateEl.addEventListener("change", function () {
                setAggregate(colIdx, aggregateEl.value);
            });

            settings$.map(function (settings) {
                return settings.aggregates[colIdx];
            }).onValue(function (value) {
                aggregateEl.value = value;
            });

            // filter
            var filterEl = $("select.futu-report-filter", control);
            if (!filterEl) return;

            filterEl.addEventListener("change", function () {
                var options = _.chain(filterEl.options)
                    .filter(function (o) { return o.selected; })
                    .map(function (o) { return JSON.parse(o.value); })
                    .value();

                setFilterBy(colIdx, options);
            });

            settings$.map(function (settings) {
                return settings.filterBy[colIdx];
            }, _.isEqual).onValue(function (values) {
                _.each(filterEl.options, function (o) {
                    var value = JSON.parse(o.value);
                    o.selected = values.indexOf(value) !== -1;
                });
            });
        });
    }

    // aggregate functions
    var aggregateFunctions = {
        first: function (x) { return x[0]; },
        sum: function (x) { return _.sum(x); },
        avg: function (x) { return _.sum(x) / x.length; },
        min: function (x) { return _.min(x); },
        max: function (x) { return _.max(x); },
        count: function (x) { return x.length; },
        countDistinct: function (x) { return _.uniq(x).length; },
        collect: function (x) { return x; },
        collectDistinct: function (x) { return _.uniq(x); },
    };

    // jQuery 4evah
    function $(selector, node) {
        node = node || document;
        return node.querySelector(selector);
    }

    function $$(selector, node) {
        node = node || document;
        return _.map(node.querySelectorAll(selector));
    }

    // Sometimes we don't care and will bail out loudly
    function assert(cond, message) {
        if (!cond) {
            console.error("assert failed", message);
            throw new Error("assert failed");
        }
    }

    // DOM Utilities
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

    // stable super sort
    // :: Array a -> (a -> b) -> (b -> b -> Ordering) -> Array a
    function superSort(inputArray, measure, comparator) {
        comparator = comparator || function (a, b) {
            if (a < b) return -1;
            if (a > b) return 1;
            return 0;
        };

        measure = measure || _.identity;

        var arr = _.map(inputArray, function (val, idx) {
            return { val: val, idx: idx, meas: measure(val) };
        });

        arr.sort(function (a, b) {
            var c = comparator(a.meas, b.meas);
            if (c === 0) return b.idx - a.idx;
            else return c;
        });

        return arr.map(function (x) { return x.val; });
    }

    // group by with _.isEqual
    // :: Array a -> (a -> b) -> [(b, NonEmpty a)]
    function superGroupBy(inputArray, measure) {
        // this is not fast, but good enough for us
        var output = [];

        _.each(inputArray, function (el) {
            var key = measure(el);

            for (var i = 0; i < output.length; i++) {
                if (_.isEqual(output[i][0], key)) {
                    output[i][1].push(el);
                    return;
                }
            }

            // not found
            output.push([key, [el]]);
        });

        return output;
    }

    // QueryString
    // this seems to work?
    function parseQueryString(qs) {
        var parts = qs.match(/[?&][^=&?]+=[^=&?]+/gi) || [];
        var result = {};
        _.each(parts, function (part) {
            var m = part.match(/^[?&](.+)=(.+)$/);
            assert(m, "parseQueryString: inconsisent matches");

            result[m[1]] = decodeURIComponent(m[2]);
        });
        return result;
    }

    // Run the main function
    main();
});
