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
import { FaGithub } from 'react-icons/fa';

export function Logo() {
  return (
    <p className="text-2xl tracking-tight text-inherit">
      <span className="font-medium text-gray-700">HOST</span>
      <span className="font-black">PATROL</span>
    </p>
  );
}

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
        <Logo />
      </NavbarBrand>

      <NavbarContent justify="end" className="space-x-4">
        {LINKS.map(({ key, title, href }) => (
          <NavbarItem key={key} className="hidden sm:flex">
            <Link href={href} className="w-full">
              {title}
            </Link>
          </NavbarItem>
        ))}

        <NavbarMenuToggle aria-label={isMenuOpen ? 'Close menu' : 'Open menu'} className="sm:hidden" />
      </NavbarContent>

      <NavbarMenu>
        {LINKS.map(({ key, title, href }) => (
          <NavbarMenuItem key={key}>
            <Link href={href} className="w-full">
              {title}
            </Link>
          </NavbarMenuItem>
        ))}
      </NavbarMenu>
    </Navbar>
  );
}

const LINKS = [
  { key: 'getting-started', title: 'Get Started', href: '/#getting-started' },
  { key: 'learn-more', title: 'Learn More', href: '/#learn-more' },
  { key: 'report', title: 'Render Report', href: '/report' },
  { key: 'github', title: <FaGithub />, href: 'https://github.com/vst/hostpatrol' },
];
