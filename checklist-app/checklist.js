document.addEventListener("DOMContentLoaded", function () {
  console.info("Initialising checklist js");

  taskEditForm();

  function taskEditForm() {
    var form = $("#futu-task-edit");
    if (!form) return;

    console.info("Initialising task editing form");

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
      alert("Task saving not implemented");
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
