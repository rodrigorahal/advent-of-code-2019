import heapq
import fileinput
from collections import defaultdict, deque
from string import ascii_uppercase


def parse():
    grid = []
    for line in fileinput.input():
        row = [char for char in line.strip("\n")]
        grid.append(row)
    return grid


def display(grid):
    for row in grid:
        print("".join(row))


def neighbors(grid, row, col):
    h, w = len(grid), len(grid[0])
    for r in [-1, 0, 1]:
        for c in [-1, 0, 1]:
            if abs(r) == abs(c):
                continue
            if 0 <= (row + r) <= h and 0 <= (col + c) <= w:
                yield row + r, col + c


def to_graph(grid):
    graph = defaultdict(list)
    for row, tiles in enumerate(grid):
        for col, tile in enumerate(tiles):
            if tile == ".":
                for rngb, cngb in neighbors(grid, row, col):
                    if grid[rngb][cngb] == ".":
                        graph[(row, col)].append((rngb, cngb))
    return graph


def find_portals(grid):
    h, w = len(grid), len(grid[0])
    portals_by_label = defaultdict(list)
    for row, tiles in enumerate(grid[: h - 2]):
        for col, tile in enumerate(tiles[: w - 2]):
            below = grid[row + 1][col]
            two_below = grid[row + 2][col]

            if (
                tile in ascii_uppercase
                and below in ascii_uppercase
                and two_below == "."
            ):
                layer = "outer" if row == 0 else "inner"
                portals_by_label[(tile, below)].append((layer, row + 2, col))

            if (
                tile == "."
                and below in ascii_uppercase
                and two_below in ascii_uppercase
            ):
                layer = "outer" if row == h - 3 else "inner"
                portals_by_label[(below, two_below)].append((layer, row, col))

            right = grid[row][col + 1]
            two_right = grid[row][col + 2]

            if (
                tile in ascii_uppercase
                and right in ascii_uppercase
                and two_right == "."
            ):
                layer = "outer" if col == 0 else "inner"
                portals_by_label[(tile, right)].append((layer, row, col + 2))

            if (
                tile == "."
                and right in ascii_uppercase
                and two_right in ascii_uppercase
            ):
                layer = "outer" if col == w - 3 else "inner"
                portals_by_label[(right, two_right)].append((layer, row, col))

    start = end = None
    portals = defaultdict(list)
    for label, coords in portals_by_label.items():
        if label == ("A", "A"):
            start = coords[0][1:]
        elif label == ("Z", "Z"):
            end = coords[0][1:]
        else:
            u, v = coords
            portals[u[1:]].append(v)
            portals[v[1:]].append(u)

    return start, end, portals


def search(graph, portals, start, end):
    queue = deque([(start, [])])
    seen = set()

    while queue:
        curr, path = queue.popleft()
        seen.add(curr)

        if curr == end:
            return path

        for adj in graph[curr]:
            if adj in seen:
                continue
            queue.append((adj, path + [adj]))

        if curr in portals:
            for adj in portals[curr]:
                adj = adj[1:]
                if adj in seen:
                    continue
                queue.append((adj, path + [adj]))


def search_with_level(graph, portals, start, end):
    queue = [(0, 0, start, [])]
    seen = set()

    while queue:
        steps, level, curr, path = heapq.heappop(queue)
        seen.add((level, *curr))

        if level > 60:
            continue

        if curr == end and level == 0:
            return path

        if curr in (start, end) and level != 0:
            continue

        for adj in graph[curr]:
            if (level, *adj) in seen:
                continue
            heapq.heappush(queue, (steps + 1, level, adj, path + [(level, *adj)]))

        if curr in portals:
            for adj in portals[curr]:
                layer = adj[0]
                adj = adj[1:]
                if level == 0 and layer == "inner":
                    continue
                next_level = level - 1 if layer == "inner" else level + 1
                if (next_level, *adj) in seen:
                    continue
                heapq.heappush(
                    queue, (steps + 1, next_level, adj, path + [(next_level, *adj)])
                )


def main():
    grid = parse()
    display(grid)
    graph = to_graph(grid)
    start, end, portals = find_portals(grid)

    path = search(graph, portals, start, end)
    print(len(path))

    path = search_with_level(graph, portals, start, end)
    print(len(path))


if __name__ == "__main__":
    main()
