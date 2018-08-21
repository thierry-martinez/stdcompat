pipeline {
    agent {
        label 'slave'
    }

    stages {
        stage('Build') {
            agent {
                dockerfile true
            }
            steps {
                sh 'autoreconf && ./configure && make'
            }
        }
        stage('Test') {
            steps {
                sh 'make tests'
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
