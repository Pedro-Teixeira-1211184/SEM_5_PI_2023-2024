import {Component, OnInit} from '@angular/core';

@Component({
    selector: 'app-home',
    templateUrl: './home.component.html',
    styleUrls: ['./home.component.css']
})
export class HomeComponent implements OnInit {
    public sidebarOpen = false;

    constructor() {
    }

    ngOnInit(): void {
    }

    public handleClick(): void {
        const sidebar = document.querySelector('.sidebar') as HTMLElement;
        const content = document.getElementById('content') as HTMLElement;
        const toggleSidebarButton = document.getElementById('toggle-sidebar-button') as HTMLElement;

        if (!sidebar || !content || !toggleSidebarButton) {
            return;
        }

        if (!this.sidebarOpen) {
            sidebar.style.left = '0';
            content.style.marginLeft = '250px';
            toggleSidebarButton.style.left = '270px';
            toggleSidebarButton.style.display = 'block';
        } else {
            sidebar.style.left = '-250px';
            content.style.marginLeft = '0';
            toggleSidebarButton.style.display = 'none';
        }

        this.sidebarOpen = !this.sidebarOpen;
    }

    public closeSidebar(): void {
        const sidebar = document.querySelector('.sidebar') as HTMLElement;
        const content = document.getElementById('content') as HTMLElement;
        const toggleSidebarButton = document.getElementById('toggle-sidebar-button') as HTMLElement;

        if (!sidebar || !content || !toggleSidebarButton) {
            return;
        }

        sidebar.style.left = '-250px';
        content.style.marginLeft = '0';
        toggleSidebarButton.style.display = 'none';
        this.sidebarOpen = false;
    }

}
