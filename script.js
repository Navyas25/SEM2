let assignments = JSON.parse(localStorage.getItem("data")) || [];

function saveData() {
  localStorage.setItem("data", JSON.stringify(assignments));
}

function addAssignment() {
  const title = document.getElementById("title")?.value;
  const subject = document.getElementById("subject")?.value;
  const deadline = document.getElementById("deadline")?.value;

  if (!title || !subject || !deadline) return;

  assignments.push({ title, subject, deadline, completed: false });

  saveData();
  window.location.href = "list.html";
}

function getDaysLeft(deadline) {
  const today = new Date();
  const due = new Date(deadline);

  today.setHours(0,0,0,0);
  due.setHours(0,0,0,0);

  return Math.ceil((due - today) / (1000*60*60*24));
}

function renderAssignments() {
  const list = document.getElementById("assignmentList");
  if (!list) return;

  list.innerHTML = "";

  assignments.forEach((a, i) => {
    let days = getDaysLeft(a.deadline);
    let color = days < 0 ? "red" : days < 3 ? "red" : days < 7 ? "yellow" : "green";
    let msg = days < 0 ? "Overdue" : days + " days left";

    let li = document.createElement("li");

    li.innerHTML = `
      <span class="${a.completed ? 'completed' : ''}">
        <strong>${a.title}</strong> (${a.subject})<br>
        ${a.deadline}<br>
        <small class="${color}">${msg}</small>
      </span>
      <button onclick="toggleComplete(${i})">Done</button>
    `;

    list.appendChild(li);
  });
}

function toggleComplete(i) {
  assignments[i].completed = !assignments[i].completed;
  saveData();
  renderAssignments();
  updateChart();
}

let chart;

function updateChart() {
  const canvas = document.getElementById("progressChart");
  if (!canvas) return;

  let done = assignments.filter(a => a.completed).length;
  let pending = assignments.length - done;

  if (chart) chart.destroy();

  chart = new Chart(canvas, {
    type: "doughnut",
    data: {
      labels: ["Completed", "Pending"],
      datasets: [{
        data: [done, pending]
      }]
    }
  });
}

function toggleTheme() {
  document.body.classList.toggle("dark");
  localStorage.setItem("theme", document.body.classList.contains("dark") ? "dark" : "light");
}

(function () {
  if (localStorage.getItem("theme") === "dark") {
    document.body.classList.add("dark");
  }
})();

renderAssignments();
updateChart();
