import matplotlib.pyplot as plt
import numpy as np

years = ('2019', '2020', '2021', '2022', '2023', '2024')
ciations = [6, 3, 23, 36, 68, 16]
color = (170 / 255, 217 / 255, 76 / 255, 1.0)
y_pos = np.arange(len(years))

fig, ax = plt.subplots(figsize=(8, 2))  # Adjust the width and height here

# Set the background color of the plot
ax.set_facecolor((13 / 255, 16 / 255, 23 / 255))

# Set the foreground color for labels and ticks
foreground_color = (191 / 255, 189 / 255, 182 / 255)
ax.tick_params(colors=foreground_color, labelcolor=foreground_color)
ax.spines['top'].set_visible(False)
ax.spines['bottom'].set_color(foreground_color)
ax.spines['left'].set_visible(False)
ax.spines['right'].set_visible(False)

# Remove grid lines altogether
ax.grid(False)

ax.barh(y_pos, ciations, align='center', color=color)
ax.set_yticks(y_pos, labels=years)
ax.invert_yaxis()

# Remove the white space around the edges of the saved plot
plt.tight_layout()

# Save the plot with transparent background and no border
plt.savefig("assets/citations.png", transparent=True, bbox_inches='tight', pad_inches=0)