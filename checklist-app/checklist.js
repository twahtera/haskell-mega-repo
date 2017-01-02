document.addEventListener("DOMContentLoaded", function () {
  console.info("Initialising checklist js");

  $$("form").forEach(function (form) {
    var formId = form.dataset.futuId;

    switch (formId) {
      case "selector": return;
      case "checklist-create": return checklistCreateForm(form);
      case "checklist-edit": return checklistEditForm(form);
      case "task-create": return taskCreateForm(form);
      case "task-edit": return taskEditForm(form);
      case "task-add": return taskAddForm(form);
      default: return unknownForm(form);
    }
  });

  function unknownForm(form) {
    console.warn("Unknown form", form);

    function disable(el) {
      el.disabled = true;
    }

    $$("button", form).forEach(disable);
    $$("input", form).forEach(disable);
    $$("select", form).forEach(disable);
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

  // Commands

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

  // Menrva helpers
  function menrvaInputValue(el) {
    var value$ = menrva.source(el.value);
    var cb = function () {
      menrva.transaction()
        .set(value$, el.value.trim())
        .commit();
    };
    el.addEventListener("keyup", cb);
    el.addEventListener("change", cb);
    return value$;
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
