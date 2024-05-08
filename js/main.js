document.addEventListener('DOMContentLoaded', () => {
    const navLinks = document.querySelectorAll('.nav-link');
    const contentDiv = document.getElementById('content');

    navLinks.forEach(link => {
        link.addEventListener('click', async (e) => {
            e.preventDefault();
            const page = link.getAttribute('data-page');
            const response = await fetch(`${page}.html`);
            const html = await response.text();
            contentDiv.innerHTML = html;

            if (page === 'about')
            {
                fetchGoogleScholarData();
                renderTimeline();
            }
        });
    });

    // Load about page by default
    contentDiv.innerHTML = await (await fetch('about.html')).text();
    fetchGoogleScholarData();
    renderTimeline();
});

async function fetchGoogleScholarData()
{
    const url = 'https://scholar.google.com/citations?user=O5eVQi0AAAAJ&hl=en';
    // Implement web scraping logic to fetch data from the Google Scholar profile
    // and update the corresponding elements in about.html
    // For demonstration purposes, we'll use placeholder data
    document.getElementById('publications').textContent = '10';
    document.getElementById('citations').textContent = '100';
    document.getElementById('h-index').textContent = '5';
    document.getElementById('i10-index').textContent = '3';
}

async function renderTimeline()
{
    const response = await fetch('data/timeline.json');
    const timelineData = await response.json();
    const timelineDiv = document.getElementById('timeline');

    timelineData.forEach((item, index) => {
        const timelineItem = document.createElement('div');
        timelineItem.classList.add('timeline-item');

        const timelineContent = document.createElement('div');
        timelineContent.classList.add('timeline-content');
        timelineContent.innerHTML = `
            <h3>${item.title}</h3>
            <p>${item.company}</p>
            <p>${item.date}</p>
            <p>${item.description}</p>
        `;

        timelineItem.appendChild(timelineContent);
        timelineDiv.appendChild(timelineItem);
    });
}