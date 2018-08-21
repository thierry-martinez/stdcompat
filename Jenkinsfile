pipeline {
    agent {
        label 'slave'
    }

    stages {
        stage('Build') {
            steps {
                sh 'autoreconf && ./configure && make'
            }
        }
        stage('Test') {
            steps {
                sh './test_all_switches_in_docker.sh'
            }
        }
        stage('Deploy') {
            steps {
                sh 'make dist'
                archiveArtifacts artifacts: '*.tar.gz', fingerprint: true
            }
        }
    }
}
