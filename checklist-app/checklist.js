document.addEventListener("DOMContentLoaded", function () {
  console.info("Initialising checklist js");

  $$("form").forEach(function (form) {
    var formId = form.dataset.futuId;

    switch (formId) {
      case "selector": return;
      case "employee-create": return employeeCreateForm(form);
      case "checklist-create": return checklistCreateForm(form);
      case "checklist-edit": return checklistEditForm(form);
      case "task-create": return taskCreateForm(form);
      case "task-edit": return taskEditForm(form);
      case "task-add": return taskAddForm(form);
      default: return unknownForm(form);
    }
  });

  $$("button[data-futu-id=task-remove]").forEach(taskRemoveBtn);
  $$("input[data-futu-id=task-done-checkbox]").forEach(taskToggleCheckbox);

  function unknownForm(form) {
    console.warn("Unknown form", form);

    function disable(el) {
      el.disabled = true;
    }

    $$("button", form).forEach(disable);
    $$("input", form).forEach(disable);
    $$("select", form).forEach(disable);
  }

  function employeeCreateForm(form) {
    console.info("Initialising employee creation form");

    var defs = {
      checklistId: { sel: "select[data-futu-id=employee-checklist" },
      firstName: { sel: "input[data-futu-id=employee-firstname", check: nonEmptyCheck },
      lastName: { sel: "input[data-futu-id=employee-lastname", check: nonEmptyCheck },
      contractType: { sel: "select[data-futu-id=employee-contract-type" },
      location: { sel: "select[data-futu-id=employee-location" },
      confirmed: { sel: "input[data-futu-id=employee-confirmed" },
      startingDay: { sel: "input[data-futu-id=employee-starting-day", check: dayCheck },
      supervisor: { sel: "input[data-futu-id=employee-supervisor", check: nonEmptyCheck },
      tribe: { sel: "input[data-futu-id=employee-tribe", check: nonEmptyCheck },
      info: { sel: "textarea[data-futu-id=employee-info" },
      phone: { sel: "input[data-futu-id=employee-phone" },
      contactEmail: { sel: "input[data-futu-id=employee-contact-email" },
      fumLogin: { sel: "input[data-futu-id=employee-fum-login" },
      hrNumber: { sel: "input[data-futu-id=employee-hr-number" },
    };

    var actions = initialiseFormDefs(defs);

    var resetBtn = $_("button[data-futu-action=reset]", form);
    initaliseResetButton(resetBtn, defs, actions);

    var submitBtn = $_("button[data-futu-action=submit]", form);
    initialiseSubmitButton(submitBtn, defs, actions, function (values) {
      var checklistId = values.checklistId;
      var edit = _.omit(values, "checklistId");
      cmdCreateEmployee(checklistId, edit);
    });
  }

  function checklistCreateForm(form) {
    console.info("Initialising checklist creation form");

    var nameEl = $_("input[data-futu-id=checklist-name]", form);

    var submitBtn = $_("button[data-futu-action=submit]", form);
    var resetBtn = $_("button[data-futu-action=reset]", form);

    var name$ = menrvaInputValue(nameEl);

    var changed$ = menrva.combine(name$, function (name) {
      return name !== "";
    });

    changed$.onValue(function (changed) {
      submitBtn.disabled = !changed;
      resetBtn.disabled = !changed;
    });

    buttonOnClick(resetBtn, function () {
      nameEl.value = "";
    });

    buttonOnClick(submitBtn, function () {
      var name = name$.value();

      cmdCreateChecklist(name);
    });
  }

  function checklistEditForm(form) {
    var checklistId = form.dataset.futuChecklistId;

    console.info("Initialising checklist editing form: " + checklistId);

    var nameEl = $_("input[data-futu-id=checklist-name]", form);

    var nameOrig = nameEl.value;

    var submitBtn = $_("button[data-futu-action=submit]", form);
    var resetBtn = $_("button[data-futu-action=reset]", form);

    var name$ = menrvaInputValue(nameEl);

    var changed$ = menrva.combine(name$, function (name) {
      return name !== nameOrig;
    });

    changed$.onValue(function (changed) {
      submitBtn.disabled = !changed;
      resetBtn.disabled = !changed;
    });

    buttonOnClick(resetBtn, function () {
      console.info("Checklist edit reset");
      nameEl.value = nameOrig;
    });

    buttonOnClick(submitBtn, function () {
      var name = name$.value();

      cmdEditChecklist(checklistId, name);
    });
  }

  function taskCreateForm(form) {
    console.info("Initialising task creation form");

    var nameEl = $_("input[data-futu-id=task-name]", form);
    var roleEl = $_("select[data-futu-id=task-role]", form);

    var submitBtn = $_("button[data-futu-action=submit]", form);
    var resetBtn = $_("button[data-futu-action=reset]", form);

    var name$ = menrvaInputValue(nameEl);
    var role$ = menrvaInputValue(roleEl);

    var changed$ = menrva.combine(name$, role$, function (name, role) {
      return name !== "" && role !== "";
    });

    changed$.onValue(function (changed) {
      submitBtn.disabled = !changed;
      resetBtn.disabled = !changed;
    });

    buttonOnClick(resetBtn, function () {
      nameEl.value = "";
      roleEl.value = "IT";
    });

    buttonOnClick(submitBtn, function () {
      var name = name$.value();
      var role = role$.value();

      var edit = {};
      edit.name = name;
      edit.role = role;

      cmdCreateTask(edit);
    });
  }

  function taskEditForm(form) {
    var taskId = form.dataset.futuTaskId;

    console.info("Initialising task editing form: " + taskId);

    var nameEl = $_("#futu-task-name", form);
    var roleEl = $_("#futu-task-role", form);

    var submitBtn = $_("button[data-futu-action=submit]", form);
    var resetBtn = $_("button[data-futu-action=reset]", form);

    // todo: check elements

    var nameOrig = nameEl.dataset.futuValue;
    var roleOrig = roleEl.dataset.futuValue;

    var name$ = menrvaInputValue(nameEl);
    var role$ = menrvaInputValue(roleEl);

    var changed$ = menrva.combine(name$, role$, function (name, role) {
      return name !== nameOrig || role !== roleOrig;
    });

    changed$.onValue(function (changed) {
      submitBtn.disabled = !changed;
      resetBtn.disabled = !changed;
    });

    buttonOnClick(resetBtn, function () {
      nameEl.value = nameOrig;
      roleEl.value = roleOrig;
    });

    buttonOnClick(submitBtn, function () {
      var name = name$.value();
      var role = role$.value();

      var edit = {};
      if (name !== nameOrig) edit.name = name;
      if (role !== nameOrig) edit.role = role;

      cmdEditTask(taskId, edit);
    });
  }

  function taskAddForm(form) {
    var checklistId = form.dataset.futuChecklistId;

    console.info("Initialising task addition form", checklistId);

    var taskEl = $_("select[data-futu-id=task-id]", form);
    var applEl = $_("input[data-futu-id=task-appliance]", form);

    var submitBtn = $_("button[data-futu-action=submit]", form);

    var task$ = menrvaInputValue(taskEl);
    var appl$ = menrvaInputValue(applEl);

    buttonOnClick(submitBtn, function () {
      var taskId = task$.value();
      var appl = appl$.value();

      cmdAddTask(checklistId, taskId, appl);
    });
  }

  function taskRemoveBtn(btn) {
    buttonOnClick(btn, function () {
      var checklistId = btn.dataset.futuChecklistId;
      var taskId = btn.dataset.futuTaskId;

      cmdRemoveTask(checklistId, taskId);
    });
  }

  function taskToggleCheckbox(chk) {
    var employeeId = chk.dataset.futuEmployee;
    var taskId = chk.dataset.futuTask;
    chk.addEventListener("change", function () {
      var done = chk.checked;

      cmdTaskDoneToggle(employeeId, taskId, done);
    });
  }

  // Commands

  function cmdCreateEmployee(checklistId, edit) {
    traceCall(cmdCreateEmployee, arguments);
    return command({
      cmd: "create-employee",
      cid: checklistId,
      edit: edit,
    }).then(function (res) {
      // TODO: popup
      console.debug(res);
    });
  }

  function cmdCreateChecklist(name) {
    console.info("cmdCreateChecklist", name);
    return command({
      cmd: "create-checklist",
      name: name,
    }).then(function (res) {
      // TODO: popup
      console.debug(res);
    });
  }

  function cmdEditChecklist(checklistId, name) {
    console.info("cmdEditChecklist", checklistId, name);
    return command({
      cmd: "rename-checklist",
      cid: checklistId,
      name: name,
    });
  }

  function cmdCreateTask(edit) {
    console.info("cmdCreateTask", edit);
    return command({
      cmd: "create-task",
      edit: edit,
    }).then(function (res) {
      // TODO: popup
      console.debug(res);
    });
  }

  function cmdEditTask(taskId, edit) {
    console.info("cmdEditTask", taskId, edit);
    return command({
      cmd: "edit-task",
      tid:  taskId,
      edit: edit,
    });
  }

  function cmdAddTask(checklistId, taskId, appliance) {
    traceCall(cmdAddTask, arguments);
    return command({
      cmd: "add-task",
      cid: checklistId,
      tid: taskId,
      appliance: appliance,
    });
  }

  function cmdRemoveTask(checklistId, taskId) {
    traceCall(cmdRemoveTask, arguments);
    return command({
      cmd: "remove-task",
      cid: checklistId,
      tid: taskId,
    });
  }

  function cmdTaskDoneToggle(employeeId, taskId, done) {
    traceCall(cmdTaskDoneToggle, arguments);
    return command({
      cmd: "task-item-toggle",
      eid: employeeId,
      tid: taskId,
      done: done ? "done" : "todo",
    });
  }

  function command(cmd) {
    var url = "/command";

    var headers = new Headers();
    headers.append("Accept", "application/json");
    headers.append("Content-Type", "application/json");

    var opts = {
      method: "POST",
      headers: headers,
      credentials: "same-origin",
      body: JSON.stringify(cmd),
    };

    return fetch(url, opts)
      .then(function (res) {
        var contentType = res.headers.get("content-type");
        if (contentType && contentType.indexOf("application/json") !== -1) {
          return res.json();
        } else {
          return res.text().then(function (txt) {
            throw new Error("Not a JSON" + txt);
          });
        }
      })
      .catch(function (exc) {
        console.error(exc);
        /*
        overlay.message.classList.add("alert");
        overlay.message.classList.remove("primary");
        overlay.message.innerHTML = "Tapahtui virhe!<br />" + exc
        overlay.overlay.style.display = "";
        */
        throw exc;
      });
  }

  // Form "library"

  function nonEmptyCheck(str) {
    return str.trim() !== "";
  }

  function dayCheck(str) {
    return !!str.match(/^\d{4}-\d\d-\d\d$/);
  }

  function initialiseFormDefs(defs) {
    _.forEach(defs, function (def, k) {
      def.el = $_(def.sel);
      def.checkbox = def.el.type === "checkbox";
      def.orig = inputValue(def.el)
      def.signal = menrvaInputValue(def.el);

      def.changed$ = def.signal.map(function (x) {
        return x != def.orig;
      });

      if (def.check) {
        def.submittable$ = def.signal.map(def.check);
      } else {
        def.submittable$ = menrva.source(true);
      }

      // dirty
      def.dirty$ = menrva.source(false);
      def.el.addEventListener("blur", function () {
        menrva.transaction([def.dirty$, true]).commit();
      });

      menrva.combine(def.dirty$, def.submittable$, function (dirty, submittable) {
        return dirty && !submittable;
      }).onValue(function (errorneous) {
        if (errorneous) {
          def.el.parentElement.classList.add("error");
          def.el.classList.add("error");
        } else {
          def.el.parentElement.classList.remove("error");
          def.el.classList.remove("error");
        }
      });
    });

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
      });
      menrva.transaction(tr).commit();
    }

    return {
      markDirty: markDirty,
      markClean: markClean,
    };
  }

  function initaliseResetButton(resetBtn, defs, actions) {
    // change handling: reset button
    var changed$ = menrva.record(_.mapValues(defs, "changed$")).map(function (rec) {
      return _.chain(rec)
        .values()
        .some()
        .value();
    });

    var dirty$ = menrva.record(_.mapValues(defs, "dirty$")).map(function (rec) {
      return _.chain(rec)
        .values()
        .some()
        .value();
    });

    menrvaSome(changed$, dirty$).onValue(function (enabled) {
      resetBtn.disabled = !enabled;
    });

    buttonOnClick(resetBtn, function () {
      actions.markClean();
      _.forEach(defs, function (def) {
        // TODO: inputValueSet
        if (def.el.type === "checkbox") {
          def.el.checked = def.orig;
        } else {
          def.el.value = def.orig;
        }
      });
    });
  }

  function initialiseSubmitButton(submitBtn, defs, actions, callback) {
    var submittable$ = menrva.record(_.mapValues(defs, "submittable$")).map(function (rec) {
      return _.chain(rec)
        .values()
        .every()
        .value();
    });

    var dirty$ = menrva.record(_.mapValues(defs, "dirty$")).map(function (rec) {
      return _.chain(rec)
        .values()
        .some()
        .value();
    });

    submittable$.onValue(function (submittable) {
      if (submittable) {
        submitBtn.classList.remove("alert");
        submitBtn.classList.add("success");
      } else {
        submitBtn.classList.add("alert");
        submitBtn.classList.remove("success");
      }
    });

    buttonOnClick(submitBtn, function () {
      if (submittable$.value()) {
        var values = _.mapValues(defs, function (def) { return def.signal.value(); });
        callback(values);
      } else {
        actions.markDirty();
      }
    });
  }

  // Menrva helpers
  function menrvaInputValue(el) {
    // if checkbox
    if (el.type === "checkbox") return menrvaCheckboxValue(el);

    // otherwise use .value
    var value$ = menrva.source(el.value);
    var cb = function () {
      menrva.transaction()
        .set(value$, inputValue(el))
        .commit();
    };
    el.addEventListener("keyup", cb);
    el.addEventListener("change", cb);
    return value$;
  }

  function menrvaCheckboxValue(el) {
    var value$ = menrva.source(el.checked);
    var cb = function () {
      menrva.transaction()
        .set(value$, el.checked)
        .commit();
    };
    el.addEventListener("keyup", cb);
    el.addEventListener("change", cb);
    return value$;
  }

  function menrvaSome() {
    var signals = _.toArray(arguments);
    return menrva.sequence(signals).map(function (values) {
      return _.some(values);
    });
  }

  // Utilities

  function $(selector, el) {
    el = el || document;
    return el.querySelector(selector, el);
  }

  // mandatory element
  function $_(selector, el) {
    el = el || document;
    res = el.querySelector(selector, el);
    assert(res, "Non-existing element for selector: " + selector);
    return res;
  }

  function $$(selector, el) {
    el = el || document;
    var res = el.querySelectorAll(selector, el);
    return Array.prototype.slice.call(res);
  }

  function inputValue(el) {
    return el.type === "checkbox" ? el.checked : el.value.trim();
  }

  function assert(cond, msg) {
    if (!cond) {
      console.error(msg);
      throw new Error(msg);
    }
  }

  function traceCall(f, args) {
    var args = [f.name].concat(_.toArray(args));
    console.info.apply(console, args);
  }

  function buttonOnClick(btn, callback) {
    btn.addEventListener("click", function (e) {
      e.preventDefault();
      callback(e);
      return false;
    });
  }
});
