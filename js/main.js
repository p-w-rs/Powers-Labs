// Fetch Google Scholar information
fetch('https://scholar.google.com/citations?user=O5eVQi0AAAAJ&hl=en').then(response => response.text()).then(data => {
    const parser = new DOMParser();
    const htmlDoc = parser.parseFromString(data, 'text/html');

    const numPublications = htmlDoc.querySelector('#gsc_a_t').textContent.trim();
    const numCitations = htmlDoc.querySelector('#gsc_a_c').textContent.trim();
    const hIndex = htmlDoc.querySelector('#gsc_a_h').textContent.trim();

    document.getElementById('num-publications').textContent = numPublications;
    document.getElementById('num-citations').textContent = numCitations;
    document.getElementById('h-index').textContent = hIndex;
});

// Fetch timeline data from timeline.json
fetch('data/timeline.json').then(response => response.json()).then(data => {
    const timelineContainer = document.querySelector('.timeline-container');

    data.forEach(item => {
        const timelineItem = document.createElement('div');
        timelineItem.classList.add('timeline-item');

        const timelineIcon = document.createElement('div');
        timelineIcon.classList.add('timeline-icon');
        timelineIcon.innerHTML = '<i class="fas fa-briefcase"></i>';

        const timelineContent = document.createElement('div');
        timelineContent.classList.add('timeline-content');

        const title = document.createElement('h3');
        title.textContent = item.title;

        const company = document.createElement('p');
        company.textContent = item.company;

        const duration = document.createElement('span');
        duration.classList.add('date');
        duration.textContent = item.duration;

        const description = document.createElement('p');
        description.textContent = item.description;

        timelineContent.appendChild(title);
        timelineContent.appendChild(company);
        timelineContent.appendChild(duration);
        timelineContent.appendChild(description);

        timelineItem.appendChild(timelineIcon);
        timelineItem.appendChild(timelineContent);

        timelineContainer.appendChild(timelineItem);
    });
});