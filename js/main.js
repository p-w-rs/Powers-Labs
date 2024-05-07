// Fetch project data from GitHub repos
function fetchProjects()
{
    fetch('data/projects.json')
        .then(response => response.json())
        .then(projects => { projects.forEach(project => renderProject(project)); });
}

// Render a project card
function renderProject(project)
{
    const card = `
    <div class="ui card">
      <div class="content">
        <div class="header">${project.name}</div>
        <div class="meta">
          ${project.tags.map(tag => `<a>${tag}</a>`).join('')}
        </div>
        <div class="description">${project.summary}</div>
      </div>
      <div class="extra content">
        <span class="right floated">${project.date}</span>
        <a href="${project.url}" target="_blank">View Project</a>
      </div>
    </div>
  `;
    document.getElementById('projects').innerHTML += card;
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
    <div class="ui item">
      <div class="content">
        <a class="header" href="#" onclick="showPost('${post.id}')">${post.title}</a>
        <div class="meta">${post.date}</div>
      </div>
    </div>
  `;
    document.getElementById('blog').innerHTML += item;
}

// Show a blog post
function showPost(id)
{
    fetch(`posts/${id}.md`).then(response => response.text()).then(markdown => {
        const html = convertMarkdownToHTML(markdown);
        document.getElementById('blog').innerHTML = html;
    });
}

// Convert Markdown to HTML (using a library like marked.js)
function convertMarkdownToHTML(markdown)
{
    return marked(markdown);
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
        <a class="header">${item.title}</a>
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
    const apiUrl = `https://scholar.google.com/citations?user=${scholarId}&hl=en&cstart=0&pagesize=100`;

    fetch(apiUrl)
        .then(response => response.text())
        .then(html => {
            const parser = new DOMParser();
            const doc = parser.parseFromString(html, 'text/html');

            const citationCount = doc.querySelector('#gsc_rsb_st td:nth-child(2)').textContent;
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