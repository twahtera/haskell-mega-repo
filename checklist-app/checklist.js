document.addEventListener("DOMContentLoaded", function () {
  console.info("Initialising checklist js");

  taskEditForm();

  function taskEditForm() {
    var form = $("#futu-task-edit");
    if (!form) return;

    var taskId = form.dataset.futuTaskId;

    console.info("Initialising task editing form: " + taskId);

    var nameEl = $("#futu-task-name", form);
    var roleEl = $("#futu-task-role", form);

    var submitBtn = $("button[data-futu-action=submit]", form);
    var resetBtn = $("button[data-futu-action=reset]", form);

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

    resetBtn.addEventListener("click", function () {
      nameEl.value = nameOrig;
      roleEl.value = roleOrig;
      return false;
    });

    submitBtn.addEventListener("click", function () {
      var name = name$.value();
      var role = role$.value();

      var edit = {};
      if (name !== nameOrig) edit.name = name;
      if (role !== nameOrig) edit.role = role;

      cmdEditTask(taskId, edit);
    });
  }

  // Commands

  function cmdEditTask(taskId, edit) {
    console.info("cmdEditTask", taskId, edit);
    return command({
      cmd: "task-edit",
      edit: edit,
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
});
