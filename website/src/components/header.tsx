'use client';

import { Link } from '@nextui-org/link';
import {
  Navbar,
  NavbarBrand,
  NavbarContent,
  NavbarItem,
  NavbarMenu,
  NavbarMenuItem,
  NavbarMenuToggle,
} from '@nextui-org/navbar';
import { useState } from 'react';

export default function Header() {
  const [isMenuOpen, setIsMenuOpen] = useState(false);

  return (
    <Navbar
      maxWidth="full"
      height="3rem"
      isBordered={true}
      onMenuOpenChange={setIsMenuOpen}
      classNames={{ base: 'z-50 bg-gray-200' }}
    >
      <NavbarBrand>
        <p className="font-mono text-2xl font-bold tracking-widest text-inherit">lhp</p>
      </NavbarBrand>

      <NavbarContent className="hidden gap-4 text-sm sm:flex" justify="center">
        <NavbarItem>Lazy Hacker&apos;s Linux Host Patrol</NavbarItem>
      </NavbarContent>

      <NavbarContent justify="end">
        <NavbarItem className="hidden lg:flex">
          <Link href="https://github.com/vst/lhp" className="w-full">
            GitHub
          </Link>
        </NavbarItem>
        <NavbarMenuToggle aria-label={isMenuOpen ? 'Close menu' : 'Open menu'} className="sm:hidden" />
      </NavbarContent>

      <NavbarMenu>
        <NavbarMenuItem>
          <Link href="https://github.com/vst/lhp" className="w-full" size="lg">
            GitHub
          </Link>
        </NavbarMenuItem>
      </NavbarMenu>
    </Navbar>
  );
}
