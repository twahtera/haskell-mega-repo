lomake = (function () {
  console.info("loading lomake");

  // imports
  var $ = futu.$;
  var $_ = futu.$_;
  var $$ = futu.$$;
  var buttonOnClick = futu.buttonOnClick;

  // forms
  var forms = {};

  // initialisation
  function initialiseForm(formElement) {
    var formName = formElement.dataset.lomakeForm;
    console.info("found lomake: ", formName);

    console.log(formElement);

    // Elements
    var inputElements = $$("*[data-lomake-id");
    var resetBtn = $_("button[data-lomake-action=reset]", formElement);
    var submitBtn = $_("button[data-lomake-action=submit]", formElement);

    // Collect inputs
    var defs = {};

    _.forEach(inputElements, function (el) {
      var elName = el.dataset.lomakeId;

      // we define the def object here, so we can refer to it already.
      var def = { el: el };

      // not used atm.
      // var checkbox = def.el.type === "checkbox";

      // original value, so we can find changed elements.
      def.original$ = menrva.source(inputValue(el));
     
      // the value in the element
      def.source$ = menrvaInputValue(el);

      // todo:
      var check = undefined;

      // selectbox selector.
      // TODO: non-mandatory selectboxes.
      if (el.tagName === "SELECT") {
        check = function (value) {
          return value === "" || value === "-" ? undefined : value;
        }
      }

      // checked value
      def.signal$ = _.isFunction(check) ? def.source$.map(check) : def.source$
      
      // changed = original != source
      // note: check may say it's invalid - but it's still changed!
      def.changed$ = menrva.combine(def.original$, def.source$, function (original, source) {
        return !_.isEqual(original, source);
      });

      // submittable = signal != undefined
      // check function should return `undefined` for invalid values.
      // TODO: diagnostic reporting.
      def.submittable$ = def.signal$.map(function (x) {
        return x !== undefined;
      });

      // dirty = "touched elements".
      def.dirty$ = menrva.source(false);
      def.el.addEventListener("blur", function () {
        menrva.transaction([def.dirty$, true]).commit();
      });

      // per element validation.
      menrva.combine(def.dirty$, def.changed$, def.submittable$, function (dirty, changed, submittable) {
        if ((dirty || changed) && !submittable) {
          return "error"
        } else if (changed) {
          return "pending";
        } else {
          return false;
        }
      }).onValue(function (state) {
        if (state === "error") {
          def.el.parentElement.classList.add("error");
          def.el.classList.add("error");
          def.el.parentElement.classList.remove("pending");
          def.el.classList.remove("pending");
        } else if (state === "pending") {
          def.el.parentElement.classList.remove("error");
          def.el.classList.remove("error");
          def.el.parentElement.classList.add("pending");
          def.el.classList.add("pending");
        } else {
          def.el.parentElement.classList.remove("pending");
          def.el.classList.remove("pending");
          def.el.parentElement.classList.remove("error");
          def.el.classList.remove("error");
        }
      });
     
      defs[elName] = def;
    });

    // actions
    function markDirty() {
      var tr = [];
      _.forEach(defs, function (def) {
        tr.push(def.dirty$);
        tr.push(true);
      });
      menrva.transaction(tr).commit();
    }

    function markClean() {
      var tr = [];
      _.forEach(defs, function (def) {
        tr.push(def.dirty$);
        tr.push(false);
        tr.push(def.signal);
        tr.push(inputValue(def.el));
      });
      menrva.transaction(tr).commit();
    }

    // Form signals
    var formChanged$ = menrva.record(_.mapValues(defs, "changed$")).map(function (rec) {
      return _.chain(rec).values().some().value();
    });

    var formDirty$ = menrva.record(_.mapValues(defs, "dirty$")).map(function (rec) {
      return _.chain(rec).values().some().value();
    });

    var formSubmittable$ = menrva.record(_.mapValues(defs, "submittable$")).map(function (rec) {
      return _.chain(rec).values().every().value();
    });

    // reset button state
    menrvaSome(formChanged$, formDirty$).onValue(function (changed) {
      resetBtn.disabled = !changed;
    });

    buttonOnClick(resetBtn, function () {
      var tr = [];
      _.forEach(defs, function (def) {
        // we didn't touch the element
        tr.push(def.dirty$);
        tr.push(false);

        // source value should be what the UI shows.
        // TODO: menrva should support setting to the value of other signal!
        // https://github.com/phadej/menrva/issues/16
        tr.push(def.source$);
        tr.push(def.original$.value());
      });

      menrva.transaction(tr).commit();
    });

    // submit button state
    menrva.combine(formSubmittable$, formChanged$, function (submittable, changed) {
      return changed || !submittable;
    }).onValue(function (enabled) {
      submitBtn.disabled = !enabled;
    });

    formSubmittable$.onValue(function (submittable) {
      if (submittable) {
        submitBtn.classList.remove("alert");
        submitBtn.classList.add("success");
      } else {
        submitBtn.classList.add("alert");
        submitBtn.classList.remove("success");
      }
    });

    buttonOnClick(submitBtn, function () {
      if (formSubmittable$.value()) {
        var values = _.mapValues(defs, function (def) {
          return def.signal$.value();
        });

        // TODO:
        console.log("SUBMIT", values);
      } else {
        actions.markDirty();
      }
    });

    // write down form
    forms[formName] = {
      markDirty: markDirty,
      markClean: markClean,
    };
  };

  // onload event
  futu.onload(function () {
    console.info("initialising lomake");

    // Initialise forms.
    $$('div[data-lomake-form]').forEach(initialiseForm);
  });

  // Menrva

  // make a menrva.source with bi-directional binding.
  function menrvaInputValue(el) {
    var value$ = menrva.source(inputValue(el), _.isEqual)
    var cb = function () {
      menrva.transaction()
        .set(value$, inputValue(el))
        .commit();
    };
    el.addEventListener("keyup", cb);
    el.addEventListener("change", cb);

    value$.onValue(function (value) {
      setInputValue(el, value);
    });

    return value$;
  }

  function menrvaSome() {
    var signals = _.toArray(arguments);
    return menrva.sequence(signals).map(function (values) {
      return _.some(values);
    });
  }

  // DOM
  function inputValue(el) {
    if (el.tagName === "INPUT" && el.type === "checkbox") {
        return el.checked;
    } else if (el.tagName === "INPUT" || el.tagName === "TEXTAREA") {
        return el.value.trim();
    } else if (el.tagName === "SELECT" && el.multiple) {
        return $$("option:checked", el).map(function (o) { return o.value.trim(); });
    } else if (el.tagName === "SELECT") {
        return el.value.trim();
    } else {
        throw new Error("inputValue: how to handle " + el.tagName);
    }
  }

  function setInputValue(el, value) {
    if (el.type === "checkbox") {
      el.checked = value === true;
    } else {
      el.value = value;
    }
  }

  // exports
  return forms;
}());
