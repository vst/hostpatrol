'use client';

import {
  Navbar,
  NavbarBrand,
  NavbarContent,
  NavbarItem,
  NavbarMenu,
  NavbarMenuItem,
  NavbarMenuToggle,
} from '@nextui-org/navbar';
import Link from 'next/link';
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

      <NavbarContent className="hidden md:flex" justify="center">
        <NavbarItem>Lazy Hacker&apos;s Linux Host Patrol</NavbarItem>
      </NavbarContent>

      <NavbarContent justify="end">
        <NavbarItem className="hidden sm:flex">
          <Link href="/" className="w-full">
            Home
          </Link>
        </NavbarItem>

        <NavbarItem className="hidden sm:flex">
          <Link href="/report" className="w-full">
            Report
          </Link>
        </NavbarItem>

        <NavbarItem className="hidden sm:flex">
          <Link href="https://github.com/vst/lhp" className="w-full">
            GitHub
          </Link>
        </NavbarItem>

        <NavbarMenuToggle aria-label={isMenuOpen ? 'Close menu' : 'Open menu'} className="sm:hidden" />
      </NavbarContent>

      <NavbarMenu>
        <NavbarMenuItem>
          <Link href="/" className="w-full">
            Home
          </Link>
        </NavbarMenuItem>

        <NavbarMenuItem>
          <Link href="/report" className="w-full">
            Report
          </Link>
        </NavbarMenuItem>

        <NavbarMenuItem>
          <Link href="https://github.com/vst/lhp" className="w-full">
            GitHub
          </Link>
        </NavbarMenuItem>
      </NavbarMenu>
    </Navbar>
  );
}
