export function getCloudIconName(x: string) {
  switch (x.toLowerCase()) {
    case 'aws':
      return 'amazonec2';
    case 'do':
      return 'digitalocean';
    case 'hetzner':
      return 'hetzner';
    default:
      return 'educative';
  }
}
