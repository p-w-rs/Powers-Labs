// main.js
let projects = [];
let selectedTags = [];

// Fetch project data from GitHub repos
function fetchProjects()
{
    fetch('data/projects.json').then(response => response.json()).then(data => {
        projects = data;
        renderProjects();
    });
}

// Render project cards
function renderProjects()
{
    const projectsContainer = document.getElementById('projects');
    projectsContainer.innerHTML = '';

    const filteredProjects = projects.filter(project => {
        if (selectedTags.length === 0)
            return true;
        return selectedTags.every(tag => project.tags.includes(tag));
    });

    filteredProjects.forEach(project => {
        const card = `
      <div class="card">
        <div class="content">
          <div class="header">${project.name}</div>
          <div class="meta">
            ${project.tags.map(tag => `<a class="ui label">${tag}</a>`).join('')}
          </div>
          <div class="description">${project.summary}</div>
        </div>
        <div class="extra content">
          <span class="right floated">${project.date}</span>
          <a href="${project.url}" target="_blank">View Project</a>
          ${project.paperUrl ? `<a href="${project.paperUrl}" target="_blank">View Paper</a>` : ''}
        </div>
      </div>
    `;
        projectsContainer.innerHTML += card;
    });
}

// Sort projects by name or date
function sortProjects(criteria)
{
    if (criteria === 'name')
    {
        projects.sort((a, b) => a.name.localeCompare(b.name));
    }
    else if (criteria === 'date')
    {
        projects.sort((a, b) => new Date(b.date) - new Date(a.date));
    }
    renderProjects();
}

// Handle tag filtering
function handleTagFilter()
{
    selectedTags = Array.from(document.querySelectorAll('.ui.checkbox input:checked')).map(checkbox => checkbox.value);
    renderProjects();
}

// Fetch blog posts from the posts directory
function fetchPosts()
{
    fetch('posts/posts.json')
        .then(response => response.json())
        .then(posts => { posts.forEach(post => renderPost(post)); });
}

// Render a blog post
function renderPost(post)
{
    const item = `
    <div class="item">
      <div class="content">
        <a class="header" href="#" onclick="showPost('${post.id}')">${post.title}</a>
        <div class="description">${post.date}</div>
      </div>
    </div>
  `;
    document.getElementById('blog').innerHTML += item;
}

// Show a blog post
function showPost(id)
{
    fetch(`posts/${id}.md`).then(response => response.text()).then(markdown => {
        const html = marked(markdown);
        document.getElementById('blog').innerHTML = html;
    });
}

// Fetch timeline data from a JSON file
function fetchTimeline()
{
    fetch('data/timeline.json')
        .then(response => response.json())
        .then(items => { items.forEach(item => renderTimelineItem(item)); });
}

// Render a timeline item
function renderTimelineItem(item)
{
    const timelineItem = `
    <div class="item">
      <div class="content">
        <h3 class="header">${item.title}</h3>
        <div class="meta">${item.date}</div>
        <div class="description">${item.description}</div>
      </div>
    </div>
  `;
    document.getElementById('timeline').innerHTML += timelineItem;
}

// Fetch Google Scholar data and update the summary
function fetchScholarData()
{
    const scholarId = 'O5eVQi0AAAAJ';
    const apiUrl = `https://scholar.google.com/citations?user=${scholarId}&hl=en&view_op=list_works&sortby=pubdate`;

    fetch(apiUrl)
        .then(response => response.text())
        .then(html => {
            const parser = new DOMParser();
            const doc = parser.parseFromString(html, 'text/html');

            const citationCount = doc.querySelector('#gsc_a_ac').textContent;
            const hIndex = doc.querySelector('#gsc_rsb_st tr:nth-child(2) td:nth-child(2)').textContent;
            const i10Index = doc.querySelector('#gsc_rsb_st tr:nth-child(3) td:nth-child(2)').textContent;

            document.getElementById('citation-count').textContent = citationCount;
            document.getElementById('h-index').textContent = hIndex;
            document.getElementById('i10-index').textContent = i10Index;
        })
        .catch(error => { console.error('Error fetching Google Scholar data:', error); });
}

// Initialize
fetchProjects();
fetchPosts();
fetchTimeline();
fetchScholarData();

// Event listeners
document.querySelectorAll('.ui.checkbox')
    .forEach(checkbox => { checkbox.addEventListener('change', handleTagFilter); });